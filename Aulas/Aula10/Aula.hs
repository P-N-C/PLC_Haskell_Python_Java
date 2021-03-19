f :: t -> u -> (t, u)
f = \x y -> (x, y)

inv :: u -> t -> (t, u)
inv = \x y -> f y x