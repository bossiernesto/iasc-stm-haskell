# Ejemplos STM En Haskell

## 0. Intro a Concurrencia

Haskell soporta tanto concurrencia como paralelismo.

 * El paralelismo no afecta al sistema de tipos, justamente porque no afecta la semántica del programa. Existen dos [funciones](https://github.com/arquitecturas-concurrentes/iasc-stm-haskell/tree/master/0_par_seq):
    * `par`
    * `seq`
 * La concurrencia sí, y es explícita. La forma más básica de concurrencia se da empleando:
   * `forkIO`: ejecuta un efecto en un thread aparte. A diferencia de los threads de Java o Ruby, los threads de Haskell son más livianos y no soportan `wait`
   * `MVar`: variables mutables que soportan acceso concurrente.

Algunas nociones previas que es bueno tener en cuenta:

### Efectos

  * Haskell es un lenguaje puro, lo cual significa que no existe forma de que sus funciones, al ser evaluadas, presenten efectos, tales como mutaciones globales de estado, entrada-salida, o lanazamientos de thread. 
  * Sin emabrgo, esto no significa que no pueda utilarse para expresar lo anterior: además de sus tipos de datos Int, Float, función, etc, presenta el tipo de dato `IO`, que representa un efecto. Y dado que es un valor como cualquier otro, Haskell puede operarlo como a cualquier valor: ordenarlos, concatenerlos, etc.
  * Un programa ejecutable es una función que devuelve un valor de tipo IO. El runtime de Haskell ejecuta el efecto representado por este valor, produciendo así los efectos en el mundo real deseados.
  * Moraleja: un programa Haskell no tiene efectos, pero es capaz de devolver un valor que los representa, pudiendo asi hacer todo lo que un programa imperativo podría hacer, y más.
  * El tipo IO está cuidadosamente modelado de forma tal que sólo puede ser combinado con otros efectos. De esa forma, un efecto no puede "accidentalmente" ejecutarse dentro de una función pura.
  * Moraleja 2: En Haskell es muy fácil saber si una función tendrá efectos cuando se ejecute el efecto del `main`: basta con mirar su tipo. Si su retorno no es `IO algo`, la función jamás tendrá efectos sobre el mundo. Esto permite fácilmente separar código _"puro"_ (ahora en sentido amplio, porque como ya dijimos todo codigo es puro en Haskell) del código _"impuro"_. Esto es muy útil a la hora de trabajar con concurrencia.

### Mónadas, Do, IO

 * IO implementa la interfaz de  mónada, al igual que la lista o el Maybe. La operación de combinación `a >>= \b -> f b` construye un IO que, cuando ejecutado, ejecuta los efectos de `a` y luego los efectos de `f b`. De forma análoga `a >> b`, ejecuta `a` y luego `b`
 * Dado que el patron de control de flujo mónada es tan común en Haskell, se puede utilizar una sintaxis especial: do-syntax. Ejemplos:

```haskell
do { x } == x

do { x ; y ; z } == x >> y >> z

do { a <- b ; f a } == b >>= \a -> f a

do { a <- b ; c <- d ; f a c } == b >>= (\a -> d >>= (\c -> f a c))
```

  * La do-syntax no hace nada mágico ni nuevo. Pero a veces es mas legible cuando queremos expresar ideas que tienen un aire imperativo. Ejemplo: queremos imprimir tres lineas por pantalla. Así que escribimos una función que combina tres impresiones de linea mediante el operador `>>`:

```haskell
imprimirHolaMundo = putStrLn "hola" >> putStrLn "mundo" >> putStrLn "!"
```

Pero es más fácil verlo como tres sentencias imperativas (aun cuando realmente no lo sean!)

```haskell
do  {
  putStrLn "hola"
  putStrLn "mundo"
  putStrLn "!"
}
```

### Variables Mutables

 * Existe `IORef`: es una variable mutable como las imperativas tradicionales. La única restricción es que las funciones que operan sobre ella devuelven siempre `IO`, con lo que sólo podemos usarlas en funciones _"impuras"_

## 1. Intro a STM

ver en esta [sección](https://github.com/arquitecturas-concurrentes/iasc-stm-haskell/tree/master/1.5_stm_intro)

## 2. Cuentas no transaccionales

Aparcen algunas cosas más:

  * `async`: estructura de control que se monta arriba del thread, pero que permite
    * esperar
    * obtener un resultado


## 3. Cuentas transaccionales

Aparecen algunas cosas más:

  * `STM` y `TVar` : una expresión de tipo `STM` describe una transacción sobre un conjunto de variables transaccionales `TVar`
  * `atomically`: convierte una transacción STM en un efecto ejecutable.
