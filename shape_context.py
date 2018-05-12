import cv2
import numpy as np

# read data
datapath = "/Users/EleanorLeung/Documents/thesis"
a = cv2.imread(datapath+"/notebooks/6_ideal.png")
b = cv2.imread(datapath+"/week4/x451_0.png")

imgray_a = cv2.cvtColor(a,cv2.COLOR_BGR2GRAY)
ret_a,thresh_a = cv2.threshold(imgray_a,127,255,0)

imgray_b = cv2.cvtColor(b,cv2.COLOR_BGR2GRAY)
ret_b,thresh_b = cv2.threshold(imgray_b,127,255,0)


# find contours
_, ca, _ = cv2.findContours(thresh_a, cv2.RETR_CCOMP, cv2.CHAIN_APPROX_SIMPLE)
_, cb, _ = cv2.findContours(thresh_b, cv2.RETR_CCOMP, cv2.CHAIN_APPROX_SIMPLE)
print(np.shape(ca[0]), np.shape(cb[0]))

# generate distance --> Hausdorff OR ShapeContext
hd = cv2.createHausdorffDistanceExtractor()
sd = cv2.createShapeContextDistanceExtractor()

d1 = hd.computeDistance(ca[0],cb[0])
d2 = sd.computeDistance(ca[0],cb[0])


print(d1, " ", d2)