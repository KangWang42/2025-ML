"""
肺炎检测
基于Kaggle Chest X-Ray Images (Pneumonia)数据集
"""

import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from tqdm import tqdm
import cv2
from PIL import Image

import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import Dataset, DataLoader
import torchvision.transforms as transforms
import torchvision.models as models
from torch.optim.lr_scheduler import CosineAnnealingLR, ReduceLROnPlateau

from sklearn.model_selection import train_test_split, StratifiedKFold
from sklearn.metrics import (accuracy_score, precision_score, recall_score,
                             f1_score, roc_auc_score, confusion_matrix, classification_report)

import warnings
warnings.filterwarnings('ignore')

# 设置随机种子
def set_seed(seed=42):
    np.random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed_all(seed)
    torch.backends.cudnn.deterministic = True
    torch.backends.cudnn.benchmark = False

set_seed(42)

# ==================== 配置参数 ====================
class Config:
    # 数据路径
    DATA_DIR = '../data/chest_xray/chest_xray'  # 修改为你的数据路径
    TRAIN_DIR = os.path.join(DATA_DIR, 'train')
    VAL_DIR = os.path.join(DATA_DIR, 'val')
    TEST_DIR = os.path.join(DATA_DIR, 'test')

    # 模型参数
    MODEL_NAME = 'efficientnet_b0'  # 'resnet50', 'densenet121', 'efficientnet_b0'
    NUM_CLASSES = 2
    IMG_SIZE = 224
    BATCH_SIZE = 32
    NUM_EPOCHS = 30
    LEARNING_RATE = 1e-3
    WEIGHT_DECAY = 1e-4

    # 训练策略
    USE_PRETRAINED = True
    NUM_FOLDS = 5
    EARLY_STOPPING_PATIENCE = 7

    # 设备
    DEVICE = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    NUM_WORKERS = 4

    # 其他
    SAVE_DIR = './models'
    os.makedirs(SAVE_DIR, exist_ok=True)

config = Config()

# ==================== 数据加载与预处理 ====================

class PneumoniaDataset(Dataset):
    """肺炎检测数据集"""

    def __init__(self, image_paths, labels, transform=None):
        self.image_paths = image_paths
        self.labels = labels
        self.transform = transform

    def __len__(self):
        return len(self.image_paths)

    def __getitem__(self, idx):
        # 读取图像
        img_path = self.image_paths[idx]
        image = cv2.imread(img_path, cv2.IMREAD_GRAYSCALE)

        # 应用CLAHE增强对比度
        clahe = cv2.createCLAHE(clipLimit=2.0, tileGridSize=(8, 8))
        image = clahe.apply(image)

        # 转换为RGB (复制3通道)
        image = cv2.cvtColor(image, cv2.COLOR_GRAY2RGB)

        # PIL格式用于transforms
        image = Image.fromarray(image)

        if self.transform:
            image = self.transform(image)

        label = self.labels[idx]

        return image, label


def get_data_transforms():
    """定义数据增强策略"""

    # 训练集增强
    train_transform = transforms.Compose([
        transforms.Resize((config.IMG_SIZE, config.IMG_SIZE)),
        transforms.RandomHorizontalFlip(p=0.5),
        transforms.RandomRotation(degrees=10),
        transforms.ColorJitter(brightness=0.2, contrast=0.2),
        transforms.RandomAffine(degrees=0, translate=(0.1, 0.1)),
        transforms.ToTensor(),
        transforms.Normalize(mean=[0.485, 0.456, 0.406],
                           std=[0.229, 0.224, 0.225])
    ])

    # 验证/测试集增强
    val_transform = transforms.Compose([
        transforms.Resize((config.IMG_SIZE, config.IMG_SIZE)),
        transforms.ToTensor(),
        transforms.Normalize(mean=[0.485, 0.456, 0.406],
                           std=[0.229, 0.224, 0.225])
    ])

    return train_transform, val_transform


def load_dataset():
    """加载数据集并返回路径和标签"""

    def get_image_paths_labels(data_dir):
        image_paths = []
        labels = []

        # NORMAL类别 (标签0)
        normal_dir = os.path.join(data_dir, 'NORMAL')
        if os.path.exists(normal_dir):
            for img_name in os.listdir(normal_dir):
                if img_name.endswith(('.jpeg', '.jpg', '.png')):
                    image_paths.append(os.path.join(normal_dir, img_name))
                    labels.append(0)

        # PNEUMONIA类别 (标签1)
        pneumonia_dir = os.path.join(data_dir, 'PNEUMONIA')
        if os.path.exists(pneumonia_dir):
            for img_name in os.listdir(pneumonia_dir):
                if img_name.endswith(('.jpeg', '.jpg', '.png')):
                    image_paths.append(os.path.join(pneumonia_dir, img_name))
                    labels.append(1)

        return image_paths, labels

    # 加载训练集
    train_paths, train_labels = get_image_paths_labels(config.TRAIN_DIR)

    # 加载验证集（如果存在）
    val_paths, val_labels = get_image_paths_labels(config.VAL_DIR)

    # 加载测试集
    test_paths, test_labels = get_image_paths_labels(config.TEST_DIR)

    # 如果验证集太小，将训练集和验证集合并后重新划分
    if len(val_paths) < 100:
        train_paths = train_paths + val_paths
        train_labels = train_labels + val_labels

        train_paths, val_paths, train_labels, val_labels = train_test_split(
            train_paths, train_labels, test_size=0.2,
            random_state=42, stratify=train_labels
        )

    print(f"训练集大小: {len(train_paths)}")
    print(f"验证集大小: {len(val_paths)}")
    print(f"测试集大小: {len(test_paths)}")
    print(f"训练集类别分布: NORMAL={train_labels.count(0)}, PNEUMONIA={train_labels.count(1)}")

    return (train_paths, train_labels), (val_paths, val_labels), (test_paths, test_labels)


# ==================== 模型定义 ====================

class PneumoniaClassifier(nn.Module):
    """肺炎分类模型（基于预训练模型）"""

    def __init__(self, model_name='resnet50', num_classes=2, pretrained=True):
        super(PneumoniaClassifier, self).__init__()

        if model_name == 'resnet50':
            self.backbone = models.resnet50(pretrained=pretrained)
            num_features = self.backbone.fc.in_features
            self.backbone.fc = nn.Identity()

        elif model_name == 'densenet121':
            self.backbone = models.densenet121(pretrained=pretrained)
            num_features = self.backbone.classifier.in_features
            self.backbone.classifier = nn.Identity()

        elif model_name == 'efficientnet_b0':
            self.backbone = models.efficientnet_b0(pretrained=pretrained)
            num_features = self.backbone.classifier[1].in_features
            self.backbone.classifier = nn.Identity()

        else:
            raise ValueError(f"不支持的模型: {model_name}")

        # 自定义分类头
        self.classifier = nn.Sequential(
            nn.Linear(num_features, 512),
            nn.ReLU(),
            nn.Dropout(0.4),
            nn.Linear(512, 256),
            nn.ReLU(),
            nn.Dropout(0.3),
            nn.Linear(256, num_classes)
        )

    def forward(self, x):
        features = self.backbone(x)
        output = self.classifier(features)
        return output


# ==================== Focal Loss ====================

class FocalLoss(nn.Module):
    """Focal Loss用于处理类别不平衡"""

    def __init__(self, alpha=0.25, gamma=2.0):
        super(FocalLoss, self).__init__()
        self.alpha = alpha
        self.gamma = gamma

    def forward(self, inputs, targets):
        ce_loss = nn.CrossEntropyLoss(reduction='none')(inputs, targets)
        pt = torch.exp(-ce_loss)
        focal_loss = self.alpha * (1 - pt) ** self.gamma * ce_loss
        return focal_loss.mean()


# ==================== 训练与评估 ====================

class EarlyStopping:
    """早停机制"""

    def __init__(self, patience=7, min_delta=0, mode='min'):
        self.patience = patience
        self.min_delta = min_delta
        self.mode = mode
        self.counter = 0
        self.best_score = None
        self.early_stop = False

    def __call__(self, score):
        if self.best_score is None:
            self.best_score = score
        elif self.mode == 'min':
            if score > self.best_score - self.min_delta:
                self.counter += 1
                if self.counter >= self.patience:
                    self.early_stop = True
            else:
                self.best_score = score
                self.counter = 0
        elif self.mode == 'max':
            if score < self.best_score + self.min_delta:
                self.counter += 1
                if self.counter >= self.patience:
                    self.early_stop = True
            else:
                self.best_score = score
                self.counter = 0


def train_one_epoch(model, dataloader, criterion, optimizer, device):
    """训练一个epoch"""
    model.train()
    running_loss = 0.0
    correct = 0
    total = 0

    pbar = tqdm(dataloader, desc='Training')
    for images, labels in pbar:
        images, labels = images.to(device), labels.to(device)

        # 前向传播
        optimizer.zero_grad()
        outputs = model(images)
        loss = criterion(outputs, labels)

        # 反向传播
        loss.backward()
        optimizer.step()

        # 统计
        running_loss += loss.item() * images.size(0)
        _, predicted = outputs.max(1)
        total += labels.size(0)
        correct += predicted.eq(labels).sum().item()

        pbar.set_postfix({'loss': loss.item(), 'acc': 100. * correct / total})

    epoch_loss = running_loss / total
    epoch_acc = 100. * correct / total

    return epoch_loss, epoch_acc


def validate(model, dataloader, criterion, device):
    """验证模型"""
    model.eval()
    running_loss = 0.0
    all_preds = []
    all_labels = []
    all_probs = []

    with torch.no_grad():
        for images, labels in tqdm(dataloader, desc='Validation'):
            images, labels = images.to(device), labels.to(device)

            outputs = model(images)
            loss = criterion(outputs, labels)

            running_loss += loss.item() * images.size(0)

            probs = torch.softmax(outputs, dim=1)
            _, predicted = outputs.max(1)

            all_preds.extend(predicted.cpu().numpy())
            all_labels.extend(labels.cpu().numpy())
            all_probs.extend(probs[:, 1].cpu().numpy())  # 肺炎类别的概率

    epoch_loss = running_loss / len(dataloader.dataset)

    # 计算评估指标
    accuracy = accuracy_score(all_labels, all_preds)
    precision = precision_score(all_labels, all_preds)
    recall = recall_score(all_labels, all_preds)
    f1 = f1_score(all_labels, all_preds)
    auc = roc_auc_score(all_labels, all_probs)

    metrics = {
        'loss': epoch_loss,
        'accuracy': accuracy,
        'precision': precision,
        'recall': recall,
        'f1': f1,
        'auc': auc
    }

    return metrics, all_preds, all_labels, all_probs


def train_model(model, train_loader, val_loader, criterion, optimizer,
                scheduler, num_epochs, device, save_path):
    """完整训练流程"""

    best_auc = 0.0
    early_stopping = EarlyStopping(patience=config.EARLY_STOPPING_PATIENCE, mode='max')
    history = {'train_loss': [], 'train_acc': [], 'val_metrics': []}

    for epoch in range(num_epochs):
        print(f"\nEpoch {epoch+1}/{num_epochs}")
        print("-" * 60)

        # 训练
        train_loss, train_acc = train_one_epoch(model, train_loader, criterion, optimizer, device)

        # 验证
        val_metrics, _, _, _ = validate(model, val_loader, criterion, device)

        # 学习率调度
        if isinstance(scheduler, ReduceLROnPlateau):
            scheduler.step(val_metrics['loss'])
        else:
            scheduler.step()

        # 记录历史
        history['train_loss'].append(train_loss)
        history['train_acc'].append(train_acc)
        history['val_metrics'].append(val_metrics)

        # 打印结果
        print(f"Train Loss: {train_loss:.4f}, Train Acc: {train_acc:.2f}%")
        print(f"Val Loss: {val_metrics['loss']:.4f}, Val Acc: {val_metrics['accuracy']*100:.2f}%")
        print(f"Val Precision: {val_metrics['precision']:.4f}, Val Recall: {val_metrics['recall']:.4f}")
        print(f"Val F1: {val_metrics['f1']:.4f}, Val AUC: {val_metrics['auc']:.4f}")

        # 保存最佳模型
        if val_metrics['auc'] > best_auc:
            best_auc = val_metrics['auc']
            torch.save(model.state_dict(), save_path)
            print(f"保存最佳模型 (AUC: {best_auc:.4f})")

        # 早停检查
        early_stopping(val_metrics['auc'])
        if early_stopping.early_stop:
            print(f"Early stopping triggered at epoch {epoch+1}")
            break

    return history


# ==================== 可视化 ====================

def plot_confusion_matrix(y_true, y_pred, classes=['NORMAL', 'PNEUMONIA']):
    """绘制混淆矩阵"""
    cm = confusion_matrix(y_true, y_pred)

    plt.figure(figsize=(8, 6))
    sns.heatmap(cm, annot=True, fmt='d', cmap='Blues',
                xticklabels=classes, yticklabels=classes)
    plt.xlabel('Predicted')
    plt.ylabel('Actual')
    plt.title('Confusion Matrix')
    plt.tight_layout()
    plt.savefig('confusion_matrix.png')
    plt.close()


def plot_training_history(history):
    """绘制训练历史"""
    fig, axes = plt.subplots(2, 2, figsize=(15, 10))

    # 损失曲线
    axes[0, 0].plot(history['train_loss'], label='Train Loss')
    axes[0, 0].plot([m['loss'] for m in history['val_metrics']], label='Val Loss')
    axes[0, 0].set_xlabel('Epoch')
    axes[0, 0].set_ylabel('Loss')
    axes[0, 0].set_title('Loss Curve')
    axes[0, 0].legend()
    axes[0, 0].grid(True)

    # 准确率曲线
    axes[0, 1].plot(history['train_acc'], label='Train Acc')
    axes[0, 1].plot([m['accuracy']*100 for m in history['val_metrics']], label='Val Acc')
    axes[0, 1].set_xlabel('Epoch')
    axes[0, 1].set_ylabel('Accuracy (%)')
    axes[0, 1].set_title('Accuracy Curve')
    axes[0, 1].legend()
    axes[0, 1].grid(True)

    # AUC曲线
    axes[1, 0].plot([m['auc'] for m in history['val_metrics']], label='Val AUC')
    axes[1, 0].set_xlabel('Epoch')
    axes[1, 0].set_ylabel('AUC')
    axes[1, 0].set_title('AUC Curve')
    axes[1, 0].legend()
    axes[1, 0].grid(True)

    # F1曲线
    axes[1, 1].plot([m['f1'] for m in history['val_metrics']], label='Val F1')
    axes[1, 1].set_xlabel('Epoch')
    axes[1, 1].set_ylabel('F1 Score')
    axes[1, 1].set_title('F1 Score Curve')
    axes[1, 1].legend()
    axes[1, 1].grid(True)

    plt.tight_layout()
    plt.savefig('training_history.png')
    plt.close()


# ==================== 主函数 ====================

def main():
    """主训练流程"""

    print("=" * 60)
    print("肺炎检测 - CNN训练流程")
    print("=" * 60)

    # 1. 加载数据
    print("\n1. 加载数据...")
    (train_paths, train_labels), (val_paths, val_labels), (test_paths, test_labels) = load_dataset()

    # 2. 数据预处理
    print("\n2. 准备数据加载器...")
    train_transform, val_transform = get_data_transforms()

    train_dataset = PneumoniaDataset(train_paths, train_labels, train_transform)
    val_dataset = PneumoniaDataset(val_paths, val_labels, val_transform)
    test_dataset = PneumoniaDataset(test_paths, test_labels, val_transform)

    train_loader = DataLoader(train_dataset, batch_size=config.BATCH_SIZE,
                             shuffle=True, num_workers=config.NUM_WORKERS)
    val_loader = DataLoader(val_dataset, batch_size=config.BATCH_SIZE,
                           shuffle=False, num_workers=config.NUM_WORKERS)
    test_loader = DataLoader(test_dataset, batch_size=config.BATCH_SIZE,
                            shuffle=False, num_workers=config.NUM_WORKERS)

    # 3. 创建模型
    print(f"\n3. 创建模型: {config.MODEL_NAME}")
    model = PneumoniaClassifier(
        model_name=config.MODEL_NAME,
        num_classes=config.NUM_CLASSES,
        pretrained=config.USE_PRETRAINED
    ).to(config.DEVICE)

    print(f"模型参数量: {sum(p.numel() for p in model.parameters()) / 1e6:.2f}M")

    # 4. 定义损失函数和优化器
    print("\n4. 配置训练组件...")
    criterion = FocalLoss(alpha=0.25, gamma=2.0)
    optimizer = optim.AdamW(model.parameters(), lr=config.LEARNING_RATE,
                            weight_decay=config.WEIGHT_DECAY)
    scheduler = CosineAnnealingLR(optimizer, T_max=config.NUM_EPOCHS)

    # 5. 训练模型
    print("\n5. 开始训练...")
    save_path = os.path.join(config.SAVE_DIR, f'{config.MODEL_NAME}_best.pth')
    history = train_model(
        model, train_loader, val_loader, criterion, optimizer,
        scheduler, config.NUM_EPOCHS, config.DEVICE, save_path
    )

    # 6. 加载最佳模型并在测试集上评估
    print("\n6. 测试集评估...")
    model.load_state_dict(torch.load(save_path))
    test_metrics, test_preds, test_labels, test_probs = validate(
        model, test_loader, criterion, config.DEVICE
    )

    print("\n" + "=" * 60)
    print("测试集结果:")
    print("=" * 60)
    print(f"Accuracy: {test_metrics['accuracy']*100:.2f}%")
    print(f"Precision: {test_metrics['precision']:.4f}")
    print(f"Recall (Sensitivity): {test_metrics['recall']:.4f}")
    print(f"F1 Score: {test_metrics['f1']:.4f}")
    print(f"AUC-ROC: {test_metrics['auc']:.4f}")

    # 计算特异性
    cm = confusion_matrix(test_labels, test_preds)
    tn, fp, fn, tp = cm.ravel()
    specificity = tn / (tn + fp)
    print(f"Specificity: {specificity:.4f}")

    print("\n分类报告:")
    print(classification_report(test_labels, test_preds,
                               target_names=['NORMAL', 'PNEUMONIA']))

    # 7. 可视化结果
    print("\n7. 生成可视化图表...")
    plot_confusion_matrix(test_labels, test_preds)
    plot_training_history(history)

    print("\n训练完成! 模型已保存至:", save_path)
    print("可视化图表已保存: confusion_matrix.png, training_history.png")


if __name__ == '__main__':
    main()
