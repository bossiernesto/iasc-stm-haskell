## Introducción a STM

En el modelo tradicional de programación con threads, cuando compartimos variables o estado entre distintos threads, mantenemos la consistencia del estado utilizando locks, y notificamos a los threads de los cambios usando variables condicionales. Los MVAR's de Haskell implementan una versión mejorada de este esquema, pero sufre de los mismos problemas que en otros lenguajes

- Race conditions a los locks olvidados de liberar
- Deadlocks que resulten de locks inconsistentes
- Corrupción si hay excepciones no capturadas
- Notificaciones omitidas puede traducirse en pérdida de notificación al thread.

Estos problemas afectan a cualquier escala de software usando este esquema, por lo que es bastante complejo el manejo de threads con estado compartido.

Software transactional memory (STM) nos da unas herramientas básicas aunque potentes con las cuales podemos solucionar casi todos los problemas ya mencionados. STM ejecuta un bloque de acciones como una transacción usando un combinador llamado atomically, en suma este combinator nos permite convertir una transacción STM en un bloque ejecutable. Una vez que entramos al bloque, otros threads no pueden ver ninguna modificaciones que hagamos hasta que salgamos, y nuestro thread no puede ver ninguno de los cambios hechos por otros threads. Estas dos propiedades hacen que nuestra ejecución sea aislada. Esto nos hace pensar a algo muy similar a un mutex, en el que no se permite a otro thread modificar un estado hasta que lo libere.

![STM y mutex](mutex.png)

Cuando se sale de una transacción, solo una de las dos siguientes cosas pueden suceder:

- Si no hay otro thread que haya modificado concurrentemente el mismo estado que nosotros, todas nuestras modificaciones serán visibles automaticamente a otros threads

- Toas las modificaciones son descartadas sin ser ejecutadas, y nuestro bloque de acciones es reiniciado automáticamente.

La naturaleza de ejecutar todo o nada del bloque automatically se ejecuta de manera atómica, por eso se llama asi :P .
Esto es una de las propiedades ACID que vemos en las bases de datos, y es por eso que trabajando con STM se ve que es algo medianamente similar aunque más simple.

Veamos el prototipo de automatically

~~~haskell
ghci> :type atomically
atomically :: STM a -> IO a
~~~

Como podemos hacer un fork dentro de un bloque atomically?
De la siguiente manera

~~~haskell
atomically $ do
  leftFork <- takeFork left
  rightFork <- takeFork right
~~~

STM tiene una construcción llamada TVars que contienen un estado compartido, cuando un thread ejecuta una transacción, crea un log para el solo, en STM tenemos dos operaciones que son writeTVar y readTVar, cuando se modifica una TVar por medio de writeTVar, no se pisa este valor pero se lo registra en el log, o sea el log es como un diario que solo usaremos dentro del contexto de la transacción para un thread en particular, y se desecha al salir del bloque de la transacción. Ya vimos que pasa cuando queremos escribir un Tvar, y cuando queremos leerlo dentro de la ransacción?

veamos las firmas de estas funciones:

~~~haskell
ghci> :type newTVar
newTVar :: a -> STM (TVar a)
ghci> :type readTVar
readTVar :: TVar a -> STM a
ghci> :type writeTVar
writeTVar :: TVar a -> a -> STM ()
~~~

Cuando queremos ejecutar readTvar en una transacción pasa lo siguiente:

- El thread buscará en el log por el valor, si es que fue modificado previamente por un writeTVar
- Si no se encuentra nada, el valor se lee desde el TVar directamnete, y el valor leido es grabado en el log.

Cuando el bloque atomically finaliza, el log es validado, porque puede pasar que una variable ya fue escrita por otros threads. El funcionamiento es el siguiente: Chequeamos los valores leidos por medio de readTVar registrados en el log y nos aseguramos que matchee con el valor en el TVar real, si coinciden la validación pasa, y escribimos los nuevos valores de la transacción en los TVars, sino se desecha la transacción y se reinicia, como lo explicamos previamente.

Como estamos en Haskell y tenemos transparencia referencial, nos podemos garantizar que no tenemos efecto de lado y que estaremos devolviendo el mismo resultado que esperamos tener despues de ejecutar de nuevo la transacción.

Tenemos otra construcción más que son las TMVar, que son como una caja que puede contener un valor o nada, es decir, esto es una caja casi igual a un Maybe, solamente que para STM.

para escribir un TMVar tenemos putTMVar y para leer takeTMVar

Casos a mencionar sobre TMVar con put y take:

- Si se quiere poner un valor en una TMVar y ya hay un valor, putTMVar se bloqueará hasta que este vacío.
- Si se quiere leer un valor de una TMVar y esta vacío, takeTMVar se bloqueará hasta que haya un valor.

## Ejercicio

Vemos el ejercicio de Cuentas o el de Times Of Lore


### Times of lore

Este es el modelaje de un juego de un cabllero que debe salvar a un reino en un tiempo de crisis. No nos importa mucho el modelo, solamente que tenemos un personaje que tiene una HP determinado, un inventario y una cantidad de oro determinado y puede interactuar con otros personajes NPC.

Dicho esto, solo modelaremos la parte de lo que son las transacciones de items, oro, compra/venta de items y no mucho más, para empezar modelemos al personaje.

~~~haskell
data Item = RedScroll
          | BlueScroll
          | Axe
            deriving (Eq, Ord, Show)

newtype Gold = Gold Int
    deriving (Eq, Ord, Show, Num)

newtype HitPoint = HitPoint Int
    deriving (Eq, Ord, Show, Num)

type Inventory = TVar [Item]
type Health = TVar HitPoint
type Balance = TVar Gold

data Player = Player {
      balance :: Balance,
      health :: Health,
      inventory :: Inventory
    }
~~~

Modelamos tipos para el balance de oro, salud y los items, ahora todos son TVars, porque pensemos que cuando querramos hacer una compra o una venta de un item o intercambio de oro del banco al personaje, esta sea atómica, porque en el caso de la transferencia puede haber otras transacciones mutuas y el banco puede quedarse sin oro!, o cuando se quiera vender el que compra no puede tener el oro necesario.

veamos primero la transferencia de oro

~~~haskell
transfer :: Gold -> Balance -> Balance -> STM ()
transfer qty fromBal toBal = do
  fromQty <- readTVar fromBal
  toQty   <- readTVar toBal
  writeTVar fromBal (fromQty - qty)
  writeTVar toBal   (toQty + qty)
~~~

probando con lo que tenemos en transfer_test.hs, esto funcione.

la idea de usar Tvars es que si el caballero y otra persona efectuan una transferencia el programa no vea un saldo que no es el definitivo despues de la transferencia, entonces mediante STM nos permitimos tener una garantía de atomicidad y ailamiento, que si otro thread quiera ver el balance de los dos personajes, si ve el de uno vea el del otro o bien antes o después de la transferencia.

tengamos en cuenta que este estado no es puro, ya que hay efecto de lado y esto se ve reflejado en las TVars, que son mutables, entonces todo aquello que no este afectado por STM es código puro y lo que tiene estado mutable no lo es, por lo que la diferencia se hace notable y podemos decir que esta bien separado aquellas funciones que son puras y las que no lo son. Siempre y cuando podamos escribir una función pura, es buena práctica hacerlo sin recurrir a un uso de efecto de lado, por ej. cuando queremos remover un item del inventario de nuestro caballero, porque puede tener un scroll que haya usado y sea descartable, o cuando de un objeto a otro personaje, esto genera un efecto, pero en si la operación puede que sea exitosa, el item estaba en poder del caballero como no, entonces esto nos empieza de nuevo a surgir a que nuestro código sea imperativo de nuevo (?). No, la idea es que si podemos escribirlo sin tener que recurrir a esto podamos hacerlo con otras construcciones que hayamos visto, por ej en este caso si quiero remover un item de una lista, podemos escribir una función pura que nos devuelva un Maybe, que nos permitirá saber si el item estaba o no en su inventario original.

~~~haskell
removeInv :: Eq a => a -> [a] -> Maybe [a]
removeInv x xs =
    case takeWhile (/= x) xs of
      (_:ys) -> Just ys
      []     -> Nothing
~~~

veamos la función de dar un item a otro personaje, utilizando esta función pura ya armada, si nos devuelve un Nothing, entonces deberemos retornar Falso y cancelar la transacción, de esta manera tiene sentido nuestra lógica del juego.

~~~haskell
maybeGiveItem :: Item -> Inventory -> Inventory -> STM Bool
maybeGiveItem item fromInv toInv = do
  fromList <- readTVar fromInv
  case removeInv item fromList of
    Nothing      -> return False
    Just newList -> do
      writeTVar fromInv newList
      destItems <- readTVar toInv
      writeTVar toInv (item : destItems)
      return True
~~~

veamos que maybeGiveItem nos devuelve un boolean que si bien nos dice si fue todo bien o mal, esto no nos ayuda mucho a la hora de usarlo porque queda bastante raro, veamos el caso para la venta de un producto:

~~~haskell
maybeSellItem :: Item -> Gold -> Player -> Player -> STM Bool
maybeSellItem item price buyer seller = do
  given <- maybeGiveItem item (inventory seller) (inventory buyer)
  if given
    then do
      transfer price (balance buyer) (balance seller)
      return True
    else return False
~~~

Esto tiene la desventaja de que no solo el que llame a maybeSellItem o maybeGiveItem tenga que chequear si fue todo bien mediante un if, del resultado de maybeGiveItem, sino que ademas estamos propagando esto al que llame a maybeSellItem.

Aqui es donde podemos tan solo reutilizar las transacciones en la que se basan STM y tan solo cancelar o reiniciar la transacción luego si es que no se puede cumplir la misma, la manera de hacer esto en STM es mediante retry:

Entonces dar un item seria algo como

~~~haskell
giveItem :: Item -> Inventory -> Inventory -> STM ()
giveItem item fromInv toInv = do
  fromList <- readTVar fromInv
  case removeInv item fromList of
    Nothing -> retry
    Just newList -> do
      writeTVar fromInv newList
      readTVar toInv >>= writeTVar toInv . (item :)
~~~

la transferencia que armamos antes no chequeaba si tenia los fondos para hacer la misma del origen al destino, veamos de arreglar esto, usando retry:

~~~haskell
transferSTM :: Gold -> Balance -> Balance -> STM ()
transferSTM qty fromBal toBal = do
  fromQty <- readTVar fromBal
  when (qty > fromQty) $
    retry
  writeTVar fromBal (fromQty - qty)
  readTVar toBal >>= writeTVar toBal . (qty +)
~~~

ahora de esta manera la funcion para vender se hace más simple, veamos


~~~haskell
sellItem :: Item -> Gold -> Player -> Player -> STM ()
sellItem item price buyer seller = do
  giveItem item (inventory seller) (inventory buyer)
  transfer price (balance buyer) (balance seller)
~~~

no solo, que en vez de devolver un booleano a chequear estamos devolviendo una transaccion exitosa o no, sino que además, si la transacción no resulta por falta de item o fondos, la misma con retry se posterga a otro momento en el que se pueda hacer esto.



## Recursos

- https://wiki.haskell.org/Software_transactional_memory
- http://adit.io/posts/2013-05-15-Locks,-Actors,-And-STM-In-Pictures.html
