{-
{t = a -> b, b = c -> d,               d = e -> f, h = e, h -> g = c, i = e, i -> g -> f = a}
{t = a -> b, b = c -> d,               d = e -> f, h = e, c = h -> g, i = e, a = i -> g -> f}
{t = a -> b, b = (h -> g) -> (e -> f), d = e -> f, h = e, c = h -> g, i = e, a = i -> g -> f}
{t = a -> b, b = (e -> g) -> (e -> f), d = e -> f, h = e, c = h -> g, i = e, a = i -> g -> f}

t = (e -> g -> f) -> (e -> g) -> (e -> f)
-}
-----uke 11-----
bsum' :: Fractional t => t -> t
bsum' 1 = 1/2
bsum' n = bsum' (n+1) + (1/(n*(n+1)))

