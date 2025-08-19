import Data.Time.Clock
import System.IO (withFile, IOMode(ReadMode), hGetContents, writeFile)
import Control.Exception (IOException, try)
import Control.Concurrent (threadDelay)
import Data.Maybe (mapMaybe)
import Control.DeepSeq (deepseq)
import System.Directory (doesFileExist)

-- Definición del tipo de datos para representar la información de un libro
data Libro = Libro {
    idd :: String,
    nombre :: String,
    prestamo :: UTCTime,
    devolucion :: Maybe UTCTime
} deriving (Show, Read)

-- Registrar préstamo (con nombre)
registrarPrestamo :: String -> String -> UTCTime -> [Libro] -> [Libro]
registrarPrestamo iddLibro nombreLibro tiempo libreria =
    Libro iddLibro nombreLibro tiempo Nothing : libreria

-- Registrar devolución
registrarDevolucion :: String -> UTCTime -> [Libro] -> [Libro]
registrarDevolucion iddLibro tiempo libreria =
    map (\v -> if iddLibro == idd v then v { devolucion = Just tiempo } else v) libreria

-- Buscar libro
buscarLibro :: String -> [Libro] -> Maybe Libro
buscarLibro iddLibro libreria =
    find (\v -> iddLibro == idd v && isNothing (devolucion v)) libreria
  where
    isNothing Nothing = True
    isNothing _       = False
    find _ [] = Nothing
    find p (x:xs) | p x       = Just x
                  | otherwise = find p xs

-- Calcular tiempo en préstamo
tiempoEnPrestamo :: Libro -> UTCTime -> NominalDiffTime
tiempoEnPrestamo libro tiempoActual =
    case devolucion libro of
        Just tiempoDevolucion -> diffUTCTime tiempoDevolucion (prestamo libro)
        Nothing               -> diffUTCTime tiempoActual (prestamo libro)

-- Guardar librería (como CSV)
guardarLibreria :: [Libro] -> IO ()
guardarLibreria libreria = do
    writeFile "libreria.txt" (unlines (map mostrarLibro libreria))
    putStrLn "Libros guardados en el archivo libreria.txt."

-- Mostrar libro en formato CSV
mostrarLibro :: Libro -> String
mostrarLibro libro =
    idd libro ++ "," ++ nombre libro ++ "," ++ show (prestamo libro) ++ "," ++ maybe "Nothing" show (devolucion libro)

-- ---------- splitOn reemplazo casero ----------
splitOnComma :: String -> [String]
splitOnComma [] = [""]
splitOnComma (c:cs)
    | c == ','  = "" : rest
    | otherwise = (c : head rest) : tail rest
  where
    rest = splitOnComma cs
-------------------------------------------------

-- Cargar librería desde archivo
cargarLibro :: IO [Libro]
cargarLibro = do
    existe <- doesFileExist "libreria.txt"
    if not existe
        then return []   -- si no existe, empezamos vacío
        else withFile "libreria.txt" ReadMode $ \h -> do
            contenido <- hGetContents h
            contenido `deepseq` return ()  -- fuerza a leer todo y cerrar archivo
            let lineas = lines contenido
            return (mapMaybe parsearLibro lineas)
  where
    parsearLibro :: String -> Maybe Libro
    parsearLibro linea =
        case splitOnComma linea of
            [iddStr, nombreStr, prestamoStr, "Nothing"] ->
                Just $ Libro iddStr nombreStr (read prestamoStr) Nothing
            [iddStr, nombreStr, prestamoStr, devolucionStr] ->
                Just $ Libro iddStr nombreStr (read prestamoStr) (Just (read devolucionStr))
            _ -> Nothing

-- Ciclo principal
cicloPrincipal :: [Libro] -> IO ()
cicloPrincipal libreria = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar préstamo de libro"
    putStrLn "2. Registrar devolución de libro"
    putStrLn "3. Buscar libro por ID"
    putStrLn "4. Listar los libros prestados"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el ID del libro:"
            iddLibro <- getLine
            putStrLn "Ingrese el nombre del libro:"
            nombreLibro <- getLine
            tiempoActual <- getCurrentTime
            let libreriaActualizada = registrarPrestamo iddLibro nombreLibro tiempoActual libreria
            putStrLn $ "Libro '" ++ nombreLibro ++ "' con ID " ++ iddLibro ++ " ha sido prestado."
            guardarLibreria libreriaActualizada
            cicloPrincipal libreriaActualizada

        "2" -> do
            putStrLn "Ingrese el ID del libro:"
            iddLibro <- getLine
            tiempoActual <- getCurrentTime
            let libreriaActualizada = registrarDevolucion iddLibro tiempoActual libreria
            putStrLn $ "Libro con ID " ++ iddLibro ++ " devuelto a la biblioteca."
            guardarLibreria libreriaActualizada
            cicloPrincipal libreriaActualizada

        "3" -> do
            putStrLn "Ingrese el ID del libro:"
            iddLibro <- getLine
            case buscarLibro iddLibro libreria of
                Just libro -> do
                    tiempoActual <- getCurrentTime
                    let tiempoTotal = tiempoEnPrestamo libro tiempoActual
                    putStrLn $ "El libro '" ++ nombre libro ++ "' con ID " ++ iddLibro ++ " se encuentra en préstamo."
                    putStrLn $ "Tiempo en préstamo: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "El libro no se encuentra en préstamo."
            cicloPrincipal libreria

        "4" -> do
            putStrLn "Mostrando lista de libros en Megatinta:"
            libreriaActualizada <- cargarLibro
            mapM_ (\v -> putStrLn $
                "ID: " ++ idd v
                ++ ", Nombre: " ++ nombre v
                ++ ", Prestamo: " ++ show (prestamo v)
                ++ ", Devolucion: " ++ show (devolucion v)
                ) libreriaActualizada
            cicloPrincipal libreriaActualizada

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Intente de nuevo."
            cicloPrincipal libreria

-- Main
main :: IO ()
main = do
    libreria <- cargarLibro
    putStrLn "¡Bienvenido al Sistema de Gestión de Libros de la librería Megatintas!"
    cicloPrincipal libreria
