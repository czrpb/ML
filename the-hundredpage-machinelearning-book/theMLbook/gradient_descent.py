import sys
import numpy as np
# import matplotlib.pyplot as plt

# import matplotlib
# matplotlib.rcParams['mathtext.fontset'] = 'stix'
# matplotlib.rcParams['font.family'] = 'STIXGeneral'
# matplotlib.rcParams.update({'font.size': 18})


# def plot_original_data():
#     x, y = np.loadtxt("data.txt", delimiter= "\t", unpack = True)

#     plt.scatter(x, y, color='#1f77b4', marker='o')

#     plt.xlabel("Spendings, M$")
#     plt.ylabel("Sales, Units")
#     plt.title("Sales as a function of radio ad spendings.")
#     #plt.show()
#     fig1 = plt.gcf()
#     fig1.subplots_adjust(top = 0.98, bottom = 0.1, right = 0.98, left = 0.08, hspace = 0, wspace = 0)
#     fig1.savefig('../../Illustrations/gradient_descent-1.eps', format='eps', dpi=1000, bbox_inches = 'tight', pad_inches = 0)
#     fig1.savefig('../../Illustrations/gradient_descent-1.pdf', format='pdf', dpi=1000, bbox_inches = 'tight', pad_inches = 0)
#     fig1.savefig('../../Illustrations/gradient_descent-1.png', dpi=1000, bbox_inches = 'tight', pad_inches = 0)

def update_w_and_b(spendings, sales, w, b, alpha):
    dr_dw = 0.0
    dr_db = 0.0
    N = len(spendings)

    for i in range(N):
        spending = spendings[i]
        sale = sales[i]
        dr_dw += -2 * spending * (sale - (w * spending + b))
        dr_db += -2 * (sale - (w * spending + b))

    # update w and b
    w = w - (dr_dw/float(N)) * alpha
    b = b - (dr_db/float(N)) * alpha

    return w, b

def train(spendings, sales, w, b, alpha, epochs):
    image_counter = 2;
    for e in range(epochs):
        w, b = update_w_and_b(spendings, sales, w, b, alpha)

        # log the progress
        # if (e == 0) or (e < 3000 and e % 400 == 0) or (e % 3000 == 0):
        #     print("epoch: ", str(e), "loss: "+str(loss(spendings, sales, w, b)))
        #     print("w, b: ", w, b)
        #     plt.figure(image_counter)
        #     axes = plt.gca()
        #     axes.set_xlim([0,50])
        #     axes.set_ylim([0,30])
        #     plt.scatter(spendings, sales)
        #     X_plot = np.linspace(0,50,50)
        #     plt.plot(X_plot, X_plot*w + b)
        #     #plt.show()
        #     fig1 = plt.gcf()
        #     fig1.subplots_adjust(top = 0.98, bottom = 0.1, right = 0.98, left = 0.08, hspace = 0, wspace = 0)
        #     fig1.savefig('../../Illustrations/gradient_descent-' + str(image_counter) + '.eps', format='eps', dpi=1000, bbox_inches = 'tight', pad_inches = 0)
        #     fig1.savefig('../../Illustrations/gradient_descent-' + str(image_counter) + '.pdf', format='pdf', dpi=1000, bbox_inches = 'tight', pad_inches = 0)
        #     fig1.savefig('../../Illustrations/gradient_descent-' + str(image_counter) + '.png', dpi=1000, bbox_inches = 'tight', pad_inches = 0)
        #     image_counter += 1
    return w, b

def loss(spendings, sales, w, b):
    N = len(spendings)
    total_error = 0.0
    for i in range(N):
        total_error += (sales[i] - (w*spendings[i] + b))**2
    return total_error / N

x, y = np.loadtxt(sys.argv[1], delimiter= ",", skiprows=1, unpack = True)
w, b = train(x, y, 0.0, 0.0, 0.001, int(sys.argv[2])) #15000)
print(f"w: {w}, b: {b}")
print(f"loss: {loss(x, y, w, b)}\n")
# plot_original_data()

def predict(x, w, b):
    return w*x + b
x_new = 23.0
y_new = predict(x_new, w, b)
print(y_new)
