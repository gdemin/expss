context("subtotal")

a = 1:7
cro(net(a, Bottom = 1:2, Other = 2:3, position = "below"))
cro(net(a, Bottom = 1:2, Other = 2:3, position = "above"))
cro(net(a, Bottom = 1:2, Other = 2:3, position = "top"))
cro(net(a, Bottom = 1:2, Other = 2:3, position = "bottom"))


cro(net(a, Other = 2:3, position = "below"))
cro(net(a, Other = 2:3, position = "above"))
cro(net(a, Other = 2:3, position = "top"))
cro(net(a, Other = 2:3, position = "bottom"))


cro(net(a, 2:3, position = "below"))
cro(net(a, 2:3, position = "above"))
cro(net(a, 2:3, position = "top"))
cro(net(a, 2:3, position = "bottom"))
