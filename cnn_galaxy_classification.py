# Imports
import sys
import numpy as np
import tensorflow as tf

tf.logging.set_verbosity(tf.logging.INFO)


image_height = 50
image_width = 50
num_training_images = 45000
num_testing_images = 15000

x_pos = 5


def unison_shuffled_copies(a, b):
    assert len(a) == len(b)
    p = np.random.permutation(len(a))
    return a[p], b[p]


def get_training_data_and_labels():
    full_image_data = np.zeros(((num_training_images + num_testing_images), 2500))

    total_image_lines = np.empty(0)
    total_label_lines = np.empty(0)

    for i in range(1, 601):
        with open(f"../week9_env/data/2df.dat.y{i}") as input:
            lines = np.array(input.readlines())
        total_image_lines = np.append(total_image_lines, lines)

        with open(f"../week9_env/labels/2dfn.dat.y{i}") as input:
            labels = np.array(input.readlines())
        total_label_lines = np.append(total_label_lines, labels)

    image_counter = 0
    pixel_counter = -1
    for line in total_image_lines:
        pixel_counter += 1

        val = line.strip().split()
        full_image_data[image_counter, pixel_counter] = float(val[0])
        if pixel_counter == (image_height * image_width - 1):
            pixel_counter = -1
            image_counter += 1

    labels = np.array([int(label.strip().split()[0]) - 1 for label in total_label_lines])

    full_image_data, labels = unison_shuffled_copies(full_image_data, labels)

    train_data = full_image_data[:num_training_images]
    test_data = full_image_data[num_training_images:]

    train_labels = labels[:num_training_images]
    test_labels = labels[num_training_images:]

    return np.float32(train_data), train_labels, np.float32(test_data), test_labels


def get_testing_data_and_labels(particle_id):
    test_data = np.zeros((100, 2500))

    with open(f"../week9/envs/{particle_id}/2df.dat") as input:
        lines = input.readlines()

    with open(f"../week9/envs/{particle_id}/2dfn.dat") as input:
        labels = input.readlines()

    image_counter = 0
    pixel_counter = -1
    for line in lines:
        pixel_counter += 1

        val = line.strip().split()
        test_data[image_counter, pixel_counter] = float(val[0])
        if pixel_counter == (image_height * image_width - 1):
            pixel_counter = -1
            image_counter += 1

    test_labels = np.array([int(label.strip().split()[0]) - 1 for label in labels])

    return np.float32(test_data), test_labels


def get_ring_testing_data():
    test_data = np.zeros((1, 2500))

    with open("../week2/ring_example.dat") as sample:
        lines = sample.readlines()

    image_counter = 0
    pixel_counter = -1
    for line in lines:
        pixel_counter += 1

        val = line.strip().split()
        test_data[image_counter, pixel_counter] = float(val[0])
        if pixel_counter == (image_height * image_width - 1):
            pixel_counter = -1
            image_counter += 1

    test_labels = np.array([1])

    return np.float32(test_data), np.int32(test_labels)


def cnn_model_fn(features, labels, mode):
    """Model function for CNN."""

    # Input Layer
    input_layer = tf.reshape(features["x"], [-1, image_height, image_width, 1])

    # Convolutional Layer #1
    conv1 = tf.layers.conv2d(
      inputs=input_layer,
      filters=32,
      kernel_size=[5, 5],
      padding="same",
      activation=tf.nn.relu)

    # Pooling Layer #1
    pool1 = tf.layers.max_pooling2d(inputs=conv1, pool_size=[2, 2], strides=2)

    # Convolutional Layer #2 and Pooling Layer #2
    conv2 = tf.layers.conv2d(
      inputs=pool1,
      filters=64,
      kernel_size=[5, 5],
      padding="same",
      activation=tf.nn.relu)
    pool2 = tf.layers.max_pooling2d(inputs=conv2, pool_size=[2, 2], strides=2)

    # Convolutional Layer #3 and Pooling Layer #3
    conv3 = tf.layers.conv2d(
      inputs=pool2,
      filters=64,
      kernel_size=[3, 3],
      padding="same",
      activation=tf.nn.relu)
    pool3 = tf.layers.max_pooling2d(inputs=conv3, pool_size=[2, 2], strides=2)

    # Dense Layer
    pool3_flat = tf.reshape(pool3, [-1, 6 * 6 * 64])
    dense = tf.layers.dense(inputs=pool3_flat, units=1024, activation=tf.nn.relu)
    dropout = tf.layers.dropout(
      inputs=dense, rate=0.4, training=mode == tf.estimator.ModeKeys.TRAIN)

    # Logits Layer
    logits = tf.layers.dense(inputs=dropout, units=4)

    predictions = {
      # Generate predictions (for PREDICT and EVAL mode)
      "classes": tf.argmax(input=logits, axis=1),
      # Add `softmax_tensor` to the graph. It is used for PREDICT and by the
      # `logging_hook`.
      "probabilities": tf.nn.softmax(logits, name="softmax_tensor")
    }

    if mode == tf.estimator.ModeKeys.PREDICT:
        return tf.estimator.EstimatorSpec(mode=mode, predictions=predictions)

    # Calculate Loss (for both TRAIN and EVAL modes)
    loss = tf.losses.sparse_softmax_cross_entropy(labels=labels, logits=logits)

    # Configure the Training Op (for TRAIN mode)
    if mode == tf.estimator.ModeKeys.TRAIN:
        optimizer = tf.train.GradientDescentOptimizer(learning_rate=0.001)
        train_op = optimizer.minimize(
            loss=loss,
            global_step=tf.train.get_global_step())
        return tf.estimator.EstimatorSpec(mode=mode, loss=loss, train_op=train_op)

    # Add evaluation metrics (for EVAL mode)
    eval_metric_ops = {
      "accuracy": tf.metrics.accuracy(
          labels=labels, predictions=predictions["classes"])}

    return tf.estimator.EstimatorSpec(
      mode=mode, loss=loss, eval_metric_ops=eval_metric_ops)


def main(particle_id):
    # Load training and eval data
    # train_data, train_labels, eval_data, eval_labels = get_training_data_and_labels()
    eval_data, eval_labels = get_testing_data_and_labels(particle_id)

    # Create the Estimator
    galaxy_classifier = tf.estimator.Estimator(
      model_fn=cnn_model_fn, model_dir="./models/galaxy_classification_convnet_model_3_conv_layer_fix_im_gen")

    # Set up logging for predictions
    # Log the values in the "Softmax" tensor with label "probabilities"
    tensors_to_log = {"probabilities": "softmax_tensor"}
    logging_hook = tf.train.LoggingTensorHook(
      tensors=tensors_to_log, every_n_iter=10)

    # Train the model
    # train_input_fn = tf.estimator.inputs.numpy_input_fn(
    #   x={"x": train_data},
    #   y=train_labels,
    #   batch_size=100,
    #   num_epochs=None,
    #   shuffle=True)
    # galaxy_classifier.train(
    #   input_fn=train_input_fn,
    #   steps=10000,
    #   hooks=[logging_hook])

    # Evaluate the model and print results
    eval_input_fn = tf.estimator.inputs.numpy_input_fn(
      x={"x": eval_data},
      # y=eval_labels,
      num_epochs=1,
      shuffle=False)
    # eval_results = galaxy_classifier.evaluate(input_fn=eval_input_fn)
    pred_results = galaxy_classifier.predict(input_fn=eval_input_fn)

    predicted_classes = [p["classes"] for p in pred_results]

    # print(eval_results)
    print(predicted_classes)
    print(predicted_classes, file=open(f"../week9/envs/{particle_id}/results_{x_pos}/classification_results.txt", "w+"))
    return predicted_classes


if __name__ == "__main__":
    tf.app.run()
