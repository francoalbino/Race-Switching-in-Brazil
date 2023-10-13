#!/usr/bin/env python
# coding: utf-8

# In[3]:


import os
import cv2
import numpy as np
from sklearn.model_selection import train_test_split
import tensorflow as tf
from tensorflow.keras import layers

# Step 1: Data Preparation
black_white_folder = 'C:\\Users\\PC\\Downloads\\fotos_2020\\black_white\\black_white'
colored_folder = 'C:\\Users\\PC\\Downloads\\fotos_2020\\non_black_white\\non_black_white'

image_size = (64, 64)  # Adjust image size as per your preference
num_classes = 2  # Black and white (class 0) and colored (class 1)

def load_images(folder):
    images = []
    for filename in os.listdir(folder):
        img = cv2.imread(os.path.join(folder, filename))
        img = cv2.resize(img, image_size)
        images.append(img)
    return images

bw_images = load_images(black_white_folder)
colored_images = load_images(colored_folder)

images = np.array(bw_images + colored_images)
labels = np.array([0] * len(bw_images) + [1] * len(colored_images))

# Step 2: Data Splitting
train_images, test_images, train_labels, test_labels = train_test_split(images, labels, test_size=0.2, random_state=42)

# Step 3: Data Preprocessing
train_images = train_images.astype('float32') / 255.0
test_images = test_images.astype('float32') / 255.0

# Step 4: Model Training
model = tf.keras.Sequential([
    layers.Conv2D(32, (3, 3), activation='relu', input_shape=(image_size[0], image_size[1], 3)),
    layers.MaxPooling2D((2, 2)),
    layers.Conv2D(64, (3, 3), activation='relu'),
    layers.MaxPooling2D((2, 2)),
    layers.Conv2D(64, (3, 3), activation='relu'),
    layers.Flatten(),
    layers.Dense(64, activation='relu'),
    layers.Dense(num_classes, activation='softmax')
])

model.compile(optimizer='adam',
              loss=tf.keras.losses.SparseCategoricalCrossentropy(),
              metrics=['accuracy'])

model.fit(train_images, train_labels, epochs=10, batch_size=32, validation_data=(test_images, test_labels))

# Step 5: Model Evaluation
loss, accuracy = model.evaluate(test_images, test_labels)
print(f'Test loss: {loss}, Test accuracy: {accuracy}')


# In[5]:


# Step 6: Prediction
new_image_path = 'C:\\Users\\PC\\Downloads\\fotos_2020\\foto_cand2020_AC_div\\FAC10001060371_div.jpg'
new_image = cv2.imread(new_image_path)
new_image = cv2.resize(new_image, image_size)
new_image = np.expand_dims(new_image, axis=0)
new_image = new_image.astype('float32') / 255.0

prediction = model.predict(new_image)
prediction_class = np.argmax(prediction)

if prediction_class == 0:
    print('The image is predicted to be black and white.')
else:
    print('The image is predicted to be colored.')


# In[7]:


def predict_image_color(image_path):
    img = cv2.imread(image_path)
    img = cv2.resize(img, image_size)
    img = np.expand_dims(img, axis=0)
    img = img.astype('float32') / 255.0

    prediction = model.predict(img)
    prediction_class = np.argmax(prediction)

    if prediction_class == 0:
        return 'black_white'
    else:
        return 'colored'


# In[9]:


from shutil import copyfile


# In[11]:


image_folder = 'C:\\Users\\PC\\Downloads\\fotos_2020\\foto_cand2020_AC_div'  # Replace with the path to your image folder

black_white_output_folder = 'black_white_predict'
colored_output_folder = 'colored_predict'

# Create output folders if they don't exist
os.makedirs(black_white_output_folder, exist_ok=True)
os.makedirs(colored_output_folder, exist_ok=True)

for filename in os.listdir(image_folder):
    image_path = os.path.join(image_folder, filename)
    color_prediction = predict_image_color(image_path)
    
    if color_prediction == 'black_white':
        output_path = os.path.join(black_white_output_folder, filename)
    else:
        output_path = os.path.join(colored_output_folder, filename)
    
    # Copy the original image to the appropriate output folder
    copyfile(image_path, output_path)

