from keras.applications.vgg19 import VGG19
from keras.preprocessing import image
from keras.applications.vgg19 import preprocess_input
from keras.models import Model
from scipy.spatial import distance
import numpy as np

dir_name = "/Users/EleanorLeung/Documents/thesis"


def load_vgg19():
    # define the CNN network
    # Here we are using 19 layer CNN -VGG19 and initialising it
    # with pretrained imagenet weights
    base_model = VGG19(weights='imagenet', include_top=False, input_shape=(50, 50, 3), pooling='avg')

    # Extract features from an arbitrary intermediate layer
    # like the block4 pooling layer in VGG19
    model = Model(inputs=base_model.input, outputs=base_model.get_layer('block1_pool').output)

    return model


def extract_features(model, img_path):
    # load an image and preprocess it
    img = image.load_img(img_path, target_size=(50, 50))
    x = image.img_to_array(img)
    x = np.expand_dims(x, axis=0)
    x = preprocess_input(x)

    # get the features
    block4_pool_features = model.predict(x)

    return block4_pool_features.reshape(1, -1)


if __name__ == '__main__':
    pre_trained_model = load_vgg19()

    x = extract_features(pre_trained_model, f'{dir_name}/notebooks/6_ideal.png')

    # y = extract_features(pre_trained_model, f'{dir_name}/notebooks/6_cosine_sim.png')
    y = extract_features(pre_trained_model, f'{dir_name}/week9/results/0.png')

    sim = 1 - distance.cosine(x, y)

    print(sim)
