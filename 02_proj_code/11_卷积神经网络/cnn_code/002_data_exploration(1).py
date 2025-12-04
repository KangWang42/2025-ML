"""
数据探索脚本 - 肺炎检测数据集分析
分析Kaggle Chest X-Ray Images (Pneumonia)数据集
"""

import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from PIL import Image
import cv2
from collections import Counter
from tqdm import tqdm

# 设置样式
plt.style.use('seaborn-v0_8-darkgrid')
sns.set_palette("husl")


class DataExplorer:
    """数据探索类"""

    def __init__(self, data_dir):
        self.data_dir = data_dir
        self.train_dir = os.path.join(data_dir, 'train')
        self.val_dir = os.path.join(data_dir, 'val')
        self.test_dir = os.path.join(data_dir, 'test')

        self.data_info = {}

    def collect_data_info(self):
        """收集数据集信息"""
        print("正在收集数据集信息...")

        for split in ['train', 'val', 'test']:
            split_dir = os.path.join(self.data_dir, split)
            self.data_info[split] = {}

            for category in ['NORMAL', 'PNEUMONIA']:
                category_dir = os.path.join(split_dir, category)
                if os.path.exists(category_dir):
                    images = [f for f in os.listdir(category_dir)
                             if f.endswith(('.jpeg', '.jpg', '.png'))]
                    self.data_info[split][category] = len(images)
                else:
                    self.data_info[split][category] = 0

        return self.data_info

    def print_summary(self):
        """打印数据集摘要"""
        print("\n" + "=" * 70)
        print("数据集摘要统计")
        print("=" * 70)

        for split in ['train', 'val', 'test']:
            normal = self.data_info[split]['NORMAL']
            pneumonia = self.data_info[split]['PNEUMONIA']
            total = normal + pneumonia
            ratio = pneumonia / normal if normal > 0 else 0

            print(f"\n{split.upper()} 集:")
            print(f"  NORMAL:    {normal:4d} ({normal/total*100:.1f}%)")
            print(f"  PNEUMONIA: {pneumonia:4d} ({pneumonia/total*100:.1f}%)")
            print(f"  总计:      {total:4d}")
            print(f"  不平衡比例: {ratio:.2f}:1 (PNEUMONIA:NORMAL)")

        # 总计
        total_normal = sum(self.data_info[s]['NORMAL'] for s in ['train', 'val', 'test'])
        total_pneumonia = sum(self.data_info[s]['PNEUMONIA'] for s in ['train', 'val', 'test'])
        grand_total = total_normal + total_pneumonia

        print(f"\n总计:")
        print(f"  NORMAL:    {total_normal:4d} ({total_normal/grand_total*100:.1f}%)")
        print(f"  PNEUMONIA: {total_pneumonia:4d} ({total_pneumonia/grand_total*100:.1f}%)")
        print(f"  总计:      {grand_total:4d}")
        print("=" * 70)

    def plot_distribution(self):
        """绘制数据分布图"""
        fig, axes = plt.subplots(1, 2, figsize=(15, 5))

        # 子图1: 分类别统计
        splits = ['Train', 'Val', 'Test']
        normal_counts = [self.data_info[s.lower()]['NORMAL'] for s in splits]
        pneumonia_counts = [self.data_info[s.lower()]['PNEUMONIA'] for s in splits]

        x = np.arange(len(splits))
        width = 0.35

        axes[0].bar(x - width/2, normal_counts, width, label='NORMAL', color='#3498db')
        axes[0].bar(x + width/2, pneumonia_counts, width, label='PNEUMONIA', color='#e74c3c')

        axes[0].set_xlabel('Dataset Split', fontsize=12, fontweight='bold')
        axes[0].set_ylabel('Number of Images', fontsize=12, fontweight='bold')
        axes[0].set_title('Image Distribution by Split', fontsize=14, fontweight='bold')
        axes[0].set_xticks(x)
        axes[0].set_xticklabels(splits)
        axes[0].legend()
        axes[0].grid(axis='y', alpha=0.3)

        # 添加数值标签
        for i, (n, p) in enumerate(zip(normal_counts, pneumonia_counts)):
            axes[0].text(i - width/2, n + 20, str(n), ha='center', va='bottom', fontweight='bold')
            axes[0].text(i + width/2, p + 20, str(p), ha='center', va='bottom', fontweight='bold')

        # 子图2: 总体饼图
        total_normal = sum(normal_counts)
        total_pneumonia = sum(pneumonia_counts)

        colors = ['#3498db', '#e74c3c']
        explode = (0.05, 0.05)

        axes[1].pie([total_normal, total_pneumonia],
                   labels=['NORMAL', 'PNEUMONIA'],
                   autopct='%1.1f%%',
                   startangle=90,
                   colors=colors,
                   explode=explode,
                   textprops={'fontsize': 12, 'fontweight': 'bold'})
        axes[1].set_title('Overall Class Distribution', fontsize=14, fontweight='bold')

        plt.tight_layout()
        plt.savefig('data_distribution.png', dpi=150, bbox_inches='tight')
        plt.close()
        print("\n数据分布图已保存: data_distribution.png")

    def analyze_image_properties(self, num_samples=100):
        """分析图像属性（尺寸、像素统计等）"""
        print(f"\n正在分析图像属性（采样{num_samples}张）...")

        widths, heights, aspect_ratios = [], [], []
        mean_pixels, std_pixels = [], []

        # 从训练集采样
        train_normal = os.path.join(self.train_dir, 'NORMAL')
        train_pneumonia = os.path.join(self.train_dir, 'PNEUMONIA')

        sample_paths = []
        for category_dir in [train_normal, train_pneumonia]:
            if os.path.exists(category_dir):
                images = [os.path.join(category_dir, f)
                         for f in os.listdir(category_dir)
                         if f.endswith(('.jpeg', '.jpg', '.png'))]
                sample_paths.extend(np.random.choice(images,
                                                    min(num_samples//2, len(images)),
                                                    replace=False))

        for img_path in tqdm(sample_paths):
            img = cv2.imread(img_path, cv2.IMREAD_GRAYSCALE)
            if img is not None:
                h, w = img.shape
                widths.append(w)
                heights.append(h)
                aspect_ratios.append(w / h)
                mean_pixels.append(np.mean(img))
                std_pixels.append(np.std(img))

        # 统计信息
        stats = {
            'width': {'mean': np.mean(widths), 'std': np.std(widths),
                     'min': np.min(widths), 'max': np.max(widths)},
            'height': {'mean': np.mean(heights), 'std': np.std(heights),
                      'min': np.min(heights), 'max': np.max(heights)},
            'aspect_ratio': {'mean': np.mean(aspect_ratios), 'std': np.std(aspect_ratios)},
            'pixel_mean': {'mean': np.mean(mean_pixels), 'std': np.std(mean_pixels)},
            'pixel_std': {'mean': np.mean(std_pixels), 'std': np.std(std_pixels)}
        }

        print("\n图像属性统计:")
        print("-" * 70)
        print(f"宽度:   {stats['width']['mean']:.0f} ± {stats['width']['std']:.0f} "
              f"[{stats['width']['min']:.0f}, {stats['width']['max']:.0f}]")
        print(f"高度:   {stats['height']['mean']:.0f} ± {stats['height']['std']:.0f} "
              f"[{stats['height']['min']:.0f}, {stats['height']['max']:.0f}]")
        print(f"宽高比: {stats['aspect_ratio']['mean']:.2f} ± {stats['aspect_ratio']['std']:.2f}")
        print(f"像素均值: {stats['pixel_mean']['mean']:.1f} ± {stats['pixel_mean']['std']:.1f}")
        print(f"像素标准差: {stats['pixel_std']['mean']:.1f} ± {stats['pixel_std']['std']:.1f}")

        # 绘制分布图
        fig, axes = plt.subplots(2, 2, figsize=(15, 10))

        # 宽度分布
        axes[0, 0].hist(widths, bins=30, color='skyblue', edgecolor='black', alpha=0.7)
        axes[0, 0].axvline(np.mean(widths), color='red', linestyle='--',
                          label=f'Mean: {np.mean(widths):.0f}')
        axes[0, 0].set_xlabel('Width (pixels)', fontsize=11, fontweight='bold')
        axes[0, 0].set_ylabel('Frequency', fontsize=11, fontweight='bold')
        axes[0, 0].set_title('Image Width Distribution', fontsize=12, fontweight='bold')
        axes[0, 0].legend()
        axes[0, 0].grid(alpha=0.3)

        # 高度分布
        axes[0, 1].hist(heights, bins=30, color='lightcoral', edgecolor='black', alpha=0.7)
        axes[0, 1].axvline(np.mean(heights), color='red', linestyle='--',
                          label=f'Mean: {np.mean(heights):.0f}')
        axes[0, 1].set_xlabel('Height (pixels)', fontsize=11, fontweight='bold')
        axes[0, 1].set_ylabel('Frequency', fontsize=11, fontweight='bold')
        axes[0, 1].set_title('Image Height Distribution', fontsize=12, fontweight='bold')
        axes[0, 1].legend()
        axes[0, 1].grid(alpha=0.3)

        # 宽高比分布
        axes[1, 0].hist(aspect_ratios, bins=30, color='lightgreen', edgecolor='black', alpha=0.7)
        axes[1, 0].axvline(np.mean(aspect_ratios), color='red', linestyle='--',
                          label=f'Mean: {np.mean(aspect_ratios):.2f}')
        axes[1, 0].set_xlabel('Aspect Ratio (W/H)', fontsize=11, fontweight='bold')
        axes[1, 0].set_ylabel('Frequency', fontsize=11, fontweight='bold')
        axes[1, 0].set_title('Aspect Ratio Distribution', fontsize=12, fontweight='bold')
        axes[1, 0].legend()
        axes[1, 0].grid(alpha=0.3)

        # 像素强度分布
        axes[1, 1].hist(mean_pixels, bins=30, color='plum', edgecolor='black', alpha=0.7)
        axes[1, 1].axvline(np.mean(mean_pixels), color='red', linestyle='--',
                          label=f'Mean: {np.mean(mean_pixels):.1f}')
        axes[1, 1].set_xlabel('Mean Pixel Intensity', fontsize=11, fontweight='bold')
        axes[1, 1].set_ylabel('Frequency', fontsize=11, fontweight='bold')
        axes[1, 1].set_title('Pixel Intensity Distribution', fontsize=12, fontweight='bold')
        axes[1, 1].legend()
        axes[1, 1].grid(alpha=0.3)

        plt.tight_layout()
        plt.savefig('image_properties.png', dpi=150, bbox_inches='tight')
        plt.close()
        print("图像属性分布图已保存: image_properties.png")

        return stats

    def visualize_samples(self, num_samples=12):
        """可视化样本图像"""
        print(f"\n正在生成样本可视化（{num_samples}张）...")

        fig, axes = plt.subplots(3, 4, figsize=(16, 12))
        axes = axes.ravel()

        # 获取样本
        train_normal = os.path.join(self.train_dir, 'NORMAL')
        train_pneumonia = os.path.join(self.train_dir, 'PNEUMONIA')

        normal_images = [os.path.join(train_normal, f)
                        for f in os.listdir(train_normal)
                        if f.endswith(('.jpeg', '.jpg', '.png'))]
        pneumonia_images = [os.path.join(train_pneumonia, f)
                           for f in os.listdir(train_pneumonia)
                           if f.endswith(('.jpeg', '.jpg', '.png'))]

        # 各一半
        normal_samples = np.random.choice(normal_images, num_samples//2, replace=False)
        pneumonia_samples = np.random.choice(pneumonia_images, num_samples//2, replace=False)

        samples = list(normal_samples) + list(pneumonia_samples)
        labels = ['NORMAL'] * (num_samples//2) + ['PNEUMONIA'] * (num_samples//2)

        for idx, (img_path, label) in enumerate(zip(samples, labels)):
            img = cv2.imread(img_path, cv2.IMREAD_GRAYSCALE)

            # 应用CLAHE增强
            clahe = cv2.createCLAHE(clipLimit=2.0, tileGridSize=(8, 8))
            img_enhanced = clahe.apply(img)

            axes[idx].imshow(img_enhanced, cmap='gray')
            color = 'green' if label == 'NORMAL' else 'red'
            axes[idx].set_title(label, fontsize=12, fontweight='bold', color=color)
            axes[idx].axis('off')

            # 添加图像尺寸信息
            h, w = img.shape
            axes[idx].text(0.5, -0.05, f'{w}×{h}',
                          transform=axes[idx].transAxes,
                          ha='center', fontsize=9, color='gray')

        plt.tight_layout()
        plt.savefig('sample_images.png', dpi=150, bbox_inches='tight')
        plt.close()
        print("样本图像已保存: sample_images.png")

    def compare_normal_vs_pneumonia(self):
        """对比正常与肺炎图像的像素分布"""
        print("\n正在对比正常与肺炎图像特征...")

        train_normal = os.path.join(self.train_dir, 'NORMAL')
        train_pneumonia = os.path.join(self.train_dir, 'PNEUMONIA')

        normal_images = [os.path.join(train_normal, f)
                        for f in os.listdir(train_normal)
                        if f.endswith(('.jpeg', '.jpg', '.png'))][:50]
        pneumonia_images = [os.path.join(train_pneumonia, f)
                           for f in os.listdir(train_pneumonia)
                           if f.endswith(('.jpeg', '.jpg', '.png'))][:50]

        normal_means, normal_stds = [], []
        pneumonia_means, pneumonia_stds = [], []

        for img_path in tqdm(normal_images, desc='Normal'):
            img = cv2.imread(img_path, cv2.IMREAD_GRAYSCALE)
            if img is not None:
                normal_means.append(np.mean(img))
                normal_stds.append(np.std(img))

        for img_path in tqdm(pneumonia_images, desc='Pneumonia'):
            img = cv2.imread(img_path, cv2.IMREAD_GRAYSCALE)
            if img is not None:
                pneumonia_means.append(np.mean(img))
                pneumonia_stds.append(np.std(img))

        # 绘制对比图
        fig, axes = plt.subplots(1, 2, figsize=(14, 5))

        # 像素均值对比
        axes[0].hist(normal_means, bins=20, alpha=0.6, label='Normal', color='blue')
        axes[0].hist(pneumonia_means, bins=20, alpha=0.6, label='Pneumonia', color='red')
        axes[0].set_xlabel('Mean Pixel Intensity', fontsize=11, fontweight='bold')
        axes[0].set_ylabel('Frequency', fontsize=11, fontweight='bold')
        axes[0].set_title('Mean Pixel Intensity Comparison', fontsize=12, fontweight='bold')
        axes[0].legend()
        axes[0].grid(alpha=0.3)

        # 像素标准差对比
        axes[1].hist(normal_stds, bins=20, alpha=0.6, label='Normal', color='blue')
        axes[1].hist(pneumonia_stds, bins=20, alpha=0.6, label='Pneumonia', color='red')
        axes[1].set_xlabel('Pixel Intensity Std', fontsize=11, fontweight='bold')
        axes[1].set_ylabel('Frequency', fontsize=11, fontweight='bold')
        axes[1].set_title('Pixel Intensity Std Comparison', fontsize=12, fontweight='bold')
        axes[1].legend()
        axes[1].grid(alpha=0.3)

        plt.tight_layout()
        plt.savefig('normal_vs_pneumonia_comparison.png', dpi=150, bbox_inches='tight')
        plt.close()
        print("对比图已保存: normal_vs_pneumonia_comparison.png")

        print("\n统计对比:")
        print("-" * 70)
        print(f"NORMAL - 像素均值: {np.mean(normal_means):.1f} ± {np.std(normal_means):.1f}")
        print(f"PNEUMONIA - 像素均值: {np.mean(pneumonia_means):.1f} ± {np.std(pneumonia_means):.1f}")
        print(f"NORMAL - 像素标准差: {np.mean(normal_stds):.1f} ± {np.std(normal_stds):.1f}")
        print(f"PNEUMONIA - 像素标准差: {np.mean(pneumonia_stds):.1f} ± {np.std(pneumonia_stds):.1f}")


def main():
    """主函数"""
    print("=" * 70)
    print("肺炎检测数据集 - 探索性数据分析 (EDA)")
    print("=" * 70)

    # 设置数据路径
    data_dir = '../data/chest_xray/chest_xray'  # 修改为你的数据路径

    if not os.path.exists(data_dir):
        print(f"\n错误: 找不到数据目录 '{data_dir}'")
        print("请下载数据集并修改 data_dir 变量")
        return

    # 创建探索器
    explorer = DataExplorer(data_dir)

    # 1. 收集并打印摘要信息
    explorer.collect_data_info()
    explorer.print_summary()

    # 2. 绘制数据分布
    explorer.plot_distribution()

    # 3. 分析图像属性
    explorer.analyze_image_properties(num_samples=100)

    # 4. 可视化样本
    explorer.visualize_samples(num_samples=12)

    # 5. 对比正常与肺炎图像
    explorer.compare_normal_vs_pneumonia()

    print("\n" + "=" * 70)
    print("数据探索完成!")
    print("生成的图表:")
    print("  1. data_distribution.png - 数据分布图")
    print("  2. image_properties.png - 图像属性分布")
    print("  3. sample_images.png - 样本图像")
    print("  4. normal_vs_pneumonia_comparison.png - 类别对比")
    print("=" * 70)


if __name__ == '__main__':
    main()
