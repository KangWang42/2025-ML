"""
CNN基础概念演示代码
演示卷积、池化等基本操作
"""

import numpy as np
import matplotlib.pyplot as plt
import torch
import torch.nn as nn
import torch.nn.functional as F
from matplotlib.patches import Rectangle
import seaborn as sns

# 设置中文字体
plt.rcParams['font.sans-serif'] = ['SimHei', 'DejaVu Sans']
plt.rcParams['axes.unicode_minus'] = False

class CNNVisualization:
    """CNN基础操作可视化类"""
    
    def __init__(self):
        self.colors = ['#FF6B6B', '#4ECDC4', '#45B7D1', '#96CEB4', '#FECA57']
    
    def demonstrate_convolution(self):
        """演示卷积操作"""
        print("=" * 50)
        print("卷积操作演示")
        print("=" * 50)
        
        # 创建示例图像 (5x5)
        image = np.array([
            [1, 1, 1, 0, 0],
            [0, 1, 1, 1, 0],
            [0, 0, 1, 1, 1],
            [0, 0, 1, 1, 0],
            [0, 1, 1, 0, 0]
        ])
        
        # 定义卷积核 (3x3边缘检测)
        kernel = np.array([
            [-1, -1, -1],
            [-1,  8, -1],
            [-1, -1, -1]
        ])
        
        # 手动计算卷积
        output = self.manual_convolution(image, kernel)
        
        # 可视化
        fig, axes = plt.subplots(1, 3, figsize=(15, 4))

        # 原始图像
        im1 = axes[0].imshow(image, cmap='gray')
        axes[0].set_title('原始图像 (5×5)', fontsize=14)
        axes[0].set_xticks(np.arange(-0.5, 5, 1), minor=True)
        axes[0].set_yticks(np.arange(-0.5, 5, 1), minor=True)
        axes[0].grid(which='minor', color='white', linewidth=2)
        axes[0].tick_params(which='minor', size=0)
        axes[0].set_xticks(np.arange(0, 5, 1))
        axes[0].set_yticks(np.arange(0, 5, 1))
        plt.colorbar(im1, ax=axes[0])

        # 卷积核
        im2 = axes[1].imshow(kernel, cmap='coolwarm', vmin=-kernel.max(), vmax=kernel.max())
        axes[1].set_title('卷积核 (3×3)\n边缘检测', fontsize=14)
        axes[1].set_xticks(np.arange(-0.5, 3, 1), minor=True)
        axes[1].set_yticks(np.arange(-0.5, 3, 1), minor=True)
        axes[1].grid(which='minor', color='white', linewidth=2)
        axes[1].tick_params(which='minor', size=0)
        axes[1].set_xticks(np.arange(0, 3, 1))
        axes[1].set_yticks(np.arange(0, 3, 1))
        plt.colorbar(im2, ax=axes[1])

        # 输出特征图
        im3 = axes[2].imshow(output, cmap='coolwarm', vmin=-abs(output).max(), vmax=abs(output).max())
        axes[2].set_title('特征图 (3×3)\n卷积结果', fontsize=14)
        axes[2].set_xticks(np.arange(-0.5, 3, 1), minor=True)
        axes[2].set_yticks(np.arange(-0.5, 3, 1), minor=True)
        axes[2].grid(which='minor', color='white', linewidth=2)
        axes[2].tick_params(which='minor', size=0)
        axes[2].set_xticks(np.arange(0, 3, 1))
        axes[2].set_yticks(np.arange(0, 3, 1))
        plt.colorbar(im3, ax=axes[2])
        
        plt.tight_layout()
        plt.show()
        
        print("卷积操作说明:")
        print("1. 卷积核在图像上滑动")
        print("2. 对应位置相乘后求和")
        print("3. 生成新的特征图")
        print("4. 不同卷积核提取不同特征")
        
        return image, kernel, output
    
    def manual_convolution(self, image, kernel, stride=1):
        """手动实现卷积操作"""
        h_img, w_img = image.shape
        h_kernel, w_kernel = kernel.shape
        
        # 计算输出尺寸
        h_out = (h_img - h_kernel) // stride + 1
        w_out = (w_img - w_kernel) // stride + 1
        
        output = np.zeros((h_out, w_out))
        
        for i in range(0, h_out):
            for j in range(0, w_out):
                # 提取对应区域
                region = image[i:i+h_kernel, j:j+w_kernel]
                # 卷积运算
                output[i, j] = np.sum(region * kernel)
        
        return output
    
    def demonstrate_pooling(self):
        """演示池化操作"""
        print("\n" + "=" * 50)
        print("池化操作演示")
        print("=" * 50)
        
        # 创建示例特征图 (4x4)
        feature_map = np.array([
            [1, 3, 2, 4],
            [5, 6, 1, 2], 
            [3, 2, 8, 7],
            [1, 4, 6, 3]
        ])
        
        # 最大池化
        max_pooled = self.max_pooling(feature_map, pool_size=2, stride=2)
        
        # 平均池化
        avg_pooled = self.average_pooling(feature_map, pool_size=2, stride=2)
        
        # 可视化
        fig, axes = plt.subplots(1, 3, figsize=(15, 4))

        # 统一颜色范围
        vmin = feature_map.min()
        vmax = feature_map.max()

        # 原始特征图
        im1 = axes[0].imshow(feature_map, cmap='viridis', vmin=vmin, vmax=vmax)
        axes[0].set_title('原始特征图 (4×4)', fontsize=14)
        self.add_grid_lines(axes[0], feature_map.shape, pool_size=2)
        plt.colorbar(im1, ax=axes[0])

        # 最大池化结果
        im2 = axes[1].imshow(max_pooled, cmap='viridis', vmin=vmin, vmax=vmax)
        axes[1].set_title('最大池化 (2×2)', fontsize=14)
        axes[1].set_xticks(np.arange(-0.5, 2, 1), minor=True)
        axes[1].set_yticks(np.arange(-0.5, 2, 1), minor=True)
        axes[1].grid(which='minor', color='white', linewidth=2)
        axes[1].tick_params(which='minor', size=0)
        axes[1].set_xticks(np.arange(0, 2, 1))
        axes[1].set_yticks(np.arange(0, 2, 1))
        plt.colorbar(im2, ax=axes[1])

        # 平均池化结果
        im3 = axes[2].imshow(avg_pooled, cmap='viridis', vmin=vmin, vmax=vmax)
        axes[2].set_title('平均池化 (2×2)', fontsize=14)
        axes[2].set_xticks(np.arange(-0.5, 2, 1), minor=True)
        axes[2].set_yticks(np.arange(-0.5, 2, 1), minor=True)
        axes[2].grid(which='minor', color='white', linewidth=2)
        axes[2].tick_params(which='minor', size=0)
        axes[2].set_xticks(np.arange(0, 2, 1))
        axes[2].set_yticks(np.arange(0, 2, 1))
        plt.colorbar(im3, ax=axes[2])
        
        plt.tight_layout()
        plt.show()
        
        print("池化操作说明:")
        print("1. 最大池化: 取窗口内最大值，保留显著特征")
        print("2. 平均池化: 取窗口内平均值，平滑特征")
        print("3. 降低特征图尺寸，减少参数量")
        print("4. 增加感受野，提高计算效率")
        
        return feature_map, max_pooled, avg_pooled
    
    def add_grid_lines(self, ax, shape, pool_size):
        """为池化演示添加网格线"""
        h, w = shape
        for i in range(0, h, pool_size):
            ax.axhline(y=i-0.5, color='red', linewidth=3)
        for j in range(0, w, pool_size):
            ax.axvline(x=j-0.5, color='red', linewidth=3)
        ax.axhline(y=h-0.5, color='red', linewidth=3)
        ax.axvline(x=w-0.5, color='red', linewidth=3)
    
    def max_pooling(self, feature_map, pool_size, stride):
        """手动实现最大池化"""
        h, w = feature_map.shape
        h_out = (h - pool_size) // stride + 1
        w_out = (w - pool_size) // stride + 1
        
        output = np.zeros((h_out, w_out))
        
        for i in range(h_out):
            for j in range(w_out):
                region = feature_map[i*stride:i*stride+pool_size, 
                                   j*stride:j*stride+pool_size]
                output[i, j] = np.max(region)
        
        return output
    
    def average_pooling(self, feature_map, pool_size, stride):
        """手动实现平均池化"""
        h, w = feature_map.shape
        h_out = (h - pool_size) // stride + 1
        w_out = (w - pool_size) // stride + 1
        
        output = np.zeros((h_out, w_out))
        
        for i in range(h_out):
            for j in range(w_out):
                region = feature_map[i*stride:i*stride+pool_size,
                                   j*stride:j*stride+pool_size]
                output[i, j] = np.mean(region)
        
        return output
    
    def visualize_feature_maps(self, model, input_image):
        """可视化CNN中间层特征图"""
        print("\n" + "=" * 50)
        print("特征图可视化")
        print("=" * 50)
        
        # 获取中间层输出
        activation = {}
        def get_activation(name):
            def hook(model, input, output):
                activation[name] = output.detach()
            return hook
        
        # 注册钩子函数
        model.conv1.register_forward_hook(get_activation('conv1'))
        model.conv2.register_forward_hook(get_activation('conv2'))
        model.conv3.register_forward_hook(get_activation('conv3'))
        
        # 前向传播
        model.eval()
        with torch.no_grad():
            output = model(input_image.unsqueeze(0))
        
        # 可视化特征图
        layers = ['conv1', 'conv2', 'conv3']
        fig, axes = plt.subplots(3, 8, figsize=(20, 12))
        
        for layer_idx, layer_name in enumerate(layers):
            feature_maps = activation[layer_name][0]  # 取第一个样本
            
            for i in range(min(8, feature_maps.shape[0])):
                ax = axes[layer_idx, i]
                feature_map = feature_maps[i].cpu().numpy()
                im = ax.imshow(feature_map, cmap='viridis')
                ax.set_title(f'{layer_name} - 通道{i}')
                ax.axis('off')
        
        plt.suptitle('CNN各层特征图可视化', fontsize=16)
        plt.tight_layout()
        plt.show()
        
        print("特征图分析:")
        print("1. 浅层特征图: 检测边缘、纹理等底层特征")
        print("2. 深层特征图: 检测更复杂的模式和对象")
        print("3. 通道数量: 随网络加深而增加")
        print("4. 空间尺寸: 通过池化逐渐减小")
    
    def compare_activation_functions(self):
        """比较不同激活函数"""
        print("\n" + "=" * 50)
        print("激活函数比较")
        print("=" * 50)
        
        x = np.linspace(-5, 5, 1000)
        
        # 定义激活函数
        relu = np.maximum(0, x)
        sigmoid = 1 / (1 + np.exp(-x))
        tanh = np.tanh(x)
        leaky_relu = np.where(x > 0, x, 0.1 * x)
        
        # 可视化
        fig, axes = plt.subplots(2, 2, figsize=(12, 10))
        axes = axes.ravel()
        
        functions = [
            (relu, 'ReLU', 'f(x) = max(0, x)'),
            (sigmoid, 'Sigmoid', 'f(x) = 1/(1+e^(-x))'),
            (tanh, 'Tanh', 'f(x) = tanh(x)'),
            (leaky_relu, 'Leaky ReLU', 'f(x) = max(0.1x, x)')
        ]
        
        for i, (func, name, formula) in enumerate(functions):
            axes[i].plot(x, func, linewidth=3, color=self.colors[i])
            axes[i].grid(True, alpha=0.3)
            axes[i].set_title(f'{name}\n{formula}', fontsize=12)
            axes[i].set_xlabel('输入')
            axes[i].set_ylabel('输出')
            axes[i].axhline(y=0, color='k', linewidth=0.5)
            axes[i].axvline(x=0, color='k', linewidth=0.5)
        
        plt.tight_layout()
        plt.show()
        
        print("激活函数特点:")
        print("• ReLU: 简单高效，解决梯度消失，但可能出现神经元死亡")
        print("• Sigmoid: 输出0-1，但存在梯度消失和计算复杂")
        print("• Tanh: 输出-1到1，零中心化，但仍有梯度消失问题")
        print("• Leaky ReLU: 改进ReLU，避免神经元死亡")
    
    def demonstrate_cnn_architecture(self):
        """演示CNN整体架构"""
        print("\n" + "=" * 50)
        print("CNN架构演示")
        print("=" * 50)
        
        # 定义网络各层尺寸变化
        layer_info = [
            ("输入图像", (224, 224, 3), "RGB图像"),
            ("卷积层1", (224, 224, 32), "32个3×3卷积核"),
            ("池化层1", (112, 112, 32), "2×2最大池化"),
            ("卷积层2", (112, 112, 64), "64个3×3卷积核"),
            ("池化层2", (56, 56, 64), "2×2最大池化"),
            ("卷积层3", (56, 56, 128), "128个3×3卷积核"),
            ("池化层3", (28, 28, 128), "2×2最大池化"),
            ("全连接层", (1, 1, 256), "展平后全连接"),
            ("输出层", (1, 1, 2), "2个类别")
        ]
        
        # 绘制架构图
        fig, ax = plt.subplots(figsize=(16, 8))
        
        x_positions = np.linspace(0, 14, len(layer_info))
        colors = ['red', 'blue', 'green', 'blue', 'green', 'blue', 'green', 'orange', 'purple']
        
        for i, ((name, (h, w, c), desc), x_pos, color) in enumerate(zip(layer_info, x_positions, colors)):
            # 绘制层的表示
            if '池化' in name:
                # 池化层用不同形状表示
                rect = Rectangle((x_pos-0.3, 2), 0.6, max(h/50, 0.5), 
                               facecolor=color, alpha=0.7)
            else:
                rect = Rectangle((x_pos-0.4, 1), 0.8, h/50, 
                               facecolor=color, alpha=0.7)
            ax.add_patch(rect)
            
            # 添加文本标注
            ax.text(x_pos, 0.5, name, ha='center', va='top', fontsize=10, 
                   rotation=45, fontweight='bold')
            ax.text(x_pos, 0.2, f'{h}×{w}×{c}', ha='center', va='top', fontsize=8)
            ax.text(x_pos, -0.1, desc, ha='center', va='top', fontsize=7, 
                   style='italic')
            
            # 添加箭头
            if i < len(layer_info) - 1:
                ax.arrow(x_pos+0.5, h/100+1.5, x_positions[i+1]-x_pos-1, 0,
                        head_width=0.1, head_length=0.1, fc='black', ec='black')
        
        ax.set_xlim(-1, 15)
        ax.set_ylim(-0.5, 6)
        ax.set_title('CNN架构示意图', fontsize=16, fontweight='bold')
        ax.axis('off')
        
        plt.tight_layout()
        plt.show()
        
        print("CNN架构要点:")
        print("1. 特征提取: 卷积层+池化层组合")
        print("2. 层次特征: 从底层到高层特征")
        print("3. 空间尺寸: 逐层减小")
        print("4. 通道数量: 逐层增加")
        print("5. 参数共享: 减少参数量")
        print("6. 局部连接: 利用空间局部性")

def create_sample_medical_images():
    """创建医学图像样本用于演示"""
    print("=" * 50)
    print("医学图像数据预处理演示")
    print("=" * 50)
    
    # 模拟胸部X光图像特点
    np.random.seed(42)
    
    # 正常X光图像 (模拟)
    normal_image = np.zeros((200, 200))
    # 添加肋骨结构
    for i in range(5):
        y = 40 + i * 30
        x = np.arange(200)
        rib = np.exp(-((x - 100)**2) / 1000) * 0.3
        normal_image[y:y+5, :] += rib
    # 添加肺部区域
    y, x = np.meshgrid(np.arange(200), np.arange(200), indexing='ij')
    lung_region = ((x-100)**2 + (y-100)**2 < 6000) & ((x-100)**2 + (y-100)**2 > 2000)
    normal_image[lung_region] = 0.2
    # 添加噪声
    normal_image += np.random.normal(0, 0.05, (200, 200))
    normal_image = np.clip(normal_image, 0, 1)
    
    # 肺炎X光图像 (模拟)
    pneumonia_image = normal_image.copy()
    # 添加炎症区域
    inflammation = ((x-120)**2 + (y-80)**2 < 1500)
    pneumonia_image[inflammation] += 0.4
    # 添加更多不规则性
    pneumonia_image += np.random.normal(0, 0.03, (200, 200))
    pneumonia_image = np.clip(pneumonia_image, 0, 1)
    
    # 可视化
    fig, axes = plt.subplots(1, 2, figsize=(12, 5))
    
    axes[0].imshow(normal_image, cmap='gray')
    axes[0].set_title('正常胸部X光图像 (模拟)', fontsize=14)
    axes[0].axis('off')
    
    axes[1].imshow(pneumonia_image, cmap='gray')
    axes[1].set_title('肺炎胸部X光图像 (模拟)', fontsize=14)
    axes[1].axis('off')
    
    plt.tight_layout()
    plt.show()
    
    print("医学图像特点:")
    print("1. 灰度图像: 大部分医学影像为单通道")
    print("2. 对比度: 不同组织具有不同密度")
    print("3. 噪声: 成像设备和环境影响")
    print("4. 变异性: 患者个体差异大")
    print("5. 标注难度: 需要专业医学知识")
    
    return normal_image, pneumonia_image

def demonstrate_data_augmentation():
    """演示数据增强技术"""
    print("\n" + "=" * 50)
    print("数据增强演示")
    print("=" * 50)
    
    # 创建示例图像
    original_image, _ = create_sample_medical_images()
    
    # 定义增强变换
    transforms_demo = {
        '原始图像': lambda x: x,
        '旋转10°': lambda x: rotate_image(x, 10),
        '水平翻转': lambda x: np.fliplr(x),
        '亮度调整': lambda x: np.clip(x * 1.3, 0, 1),
        '对比度增强': lambda x: np.clip((x - 0.5) * 1.5 + 0.5, 0, 1),
        '添加噪声': lambda x: np.clip(x + np.random.normal(0, 0.05, x.shape), 0, 1)
    }
    
    # 可视化增强效果
    fig, axes = plt.subplots(2, 3, figsize=(15, 10))
    axes = axes.ravel()
    
    for i, (name, transform) in enumerate(transforms_demo.items()):
        augmented = transform(original_image)
        axes[i].imshow(augmented, cmap='gray')
        axes[i].set_title(name, fontsize=12)
        axes[i].axis('off')
    
    plt.suptitle('数据增强技术演示', fontsize=16)
    plt.tight_layout()
    plt.show()
    
    print("数据增强的作用:")
    print("1. 增加数据量: 从有限数据生成更多样本")
    print("2. 提高泛化: 增加模型对变化的鲁棒性")
    print("3. 防止过拟合: 增加数据多样性")
    print("4. 模拟真实变化: 反映实际应用场景")

def rotate_image(image, angle):
    """简单的图像旋转函数"""
    from scipy.ndimage import rotate
    return rotate(image, angle, reshape=False, mode='constant', cval=0)

def main_demo():
    """主演示函数"""
    print("CNN基础概念课堂演示")
    print("适用于机器学习第十一章：卷积神经网络")
    print("=" * 60)
    
    # 创建可视化对象
    viz = CNNVisualization()
    
    # 1. 卷积操作演示
    viz.demonstrate_convolution()
    
    # 2. 池化操作演示  
    viz.demonstrate_pooling()
    
    # 3. 激活函数比较
    viz.compare_activation_functions()
    
    # 4. CNN架构演示
    viz.demonstrate_cnn_architecture()
    
    # 5. 医学图像示例
    create_sample_medical_images()
    
    # 6. 数据增强演示
    demonstrate_data_augmentation()
    
    print("\n" + "=" * 60)
    print("课堂演示完成!")
    print("学生可以通过这些可视化更好地理解CNN的工作原理")
    print("=" * 60)

if __name__ == "__main__":
    main_demo()