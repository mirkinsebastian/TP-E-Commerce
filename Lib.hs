type Producto = (String, Int)

precioTotal :: Fractional a => a -> a -> a -> a -> a
precioTotal precioUnitario cantidad descuento costoDeEnvio =
    aplicarCostoDeEnvio ((aplicarDescuento precioUnitario descuento) * cantidad) costoDeEnvio


productoDeElite :: Producto -> Bool
productoDeElite (nombreDeProducto, _) =
    (productoDeLujo nombreDeProducto) &&
    (productoCodiciado nombreDeProducto) &&
    (not (productoCorriente nombreDeProducto))

aplicarDescuento :: Fractional a => a -> a -> a
aplicarDescuento precio descuento =
    precio - (porcentaje precio descuento)

porcentaje :: Fractional a => a -> a -> a
porcentaje precio descuento = precio * descuento / 100

entregaSencilla :: String -> Bool
entregaSencilla = even.length

descodiciarProducto :: String -> String
descodiciarProducto = take 10

productoDeLujo :: String -> Bool
productoDeLujo nombreDeProducto =
    (elem 'x' nombreDeProducto) || (elem 'z' nombreDeProducto)


aplicarCostoDeEnvio :: Fractional a => a -> a -> a
aplicarCostoDeEnvio precio costoDeEnvio = precio + costoDeEnvio

productoCodiciado :: String -> Bool
productoCodiciado = (>10).length

productoCorriente :: String -> Bool
productoCorriente nombreDeProducto = elem (head nombreDeProducto) "aeiou"

productoXL :: Producto -> Producto
productoXL (nombreDeProducto, precioUnitario) = (nombreDeProducto ++ " XL", precioUnitario)

versionBarata :: String -> String
versionBarata = reverse.descodiciarProducto