#lang racket
(require "TDA_SYSTEM_20915795_CaicesLima.rkt")
(require "TDA_DRIVE_20915795_CaicesLima.rkt")
(require "TDA_USER_20915795_CaicesLima.rkt")
(require "TDA_FOLDER_20915795_CaicesLima.rkt")
(require "TDA_FILE_20915795_CaicesLima.rkt")

(display "SCRIPT DE PRUEBAS ENUNCIADO LAB\n
Dejé comentadas aquellas lineas en donde\nse utilizan funciones que no implementé\n\n
          Funciones no implementadas:\n
          - ren
          - dir
          - format(implementada sin prerequisito)
          - encrypt
          - decrypt
          - plus-one
          - minus-one
          - grep
          - view-trash
          - restore")


(define S0 (system "newSystem"))

;añadiendo unidades. Incluye caso S2 que intenta añadir unidad con una letra que ya existe
(define S1 ((run S0 add-drive) #\C "SO" 1000))
(define S2 ((run S1 add-drive) #\C "SO1" 3000))
(define S3 ((run S2 add-drive) #\D "Util" 2000))

;añadiendo usuarios. Incluye caso S6 que intenta registrar usuario duplicado
(define S4 ((run S3 register) "user1"))
(define S5 ((run S4 register) "user1"))
(define S6 ((run S5 register) "user2"))

;iniciando sesión con usuarios. Incluye caso S8 que intenta iniciar sesión con user2 sin antes haber salido con user1
(define S7 ((run S6 login) "user1"))
(define S8 ((run S7 login) "user2"))

;cerrando sesión user1 e iniciando con user2
(define S9 (run S8 logout))
(define S10 ((run S9 login) "user2"))

;cambios de unidad, incluyendo unidad inexistente K
(define S11 ((run S10 switch-drive) #\K))
(define S12 ((run S11 switch-drive) #\C))

;añadiendo carpetas. Incluye casos de carpetas duplicadas.
(define S13 ((run S12 md) "folder1"))
(define S14 ((run S13 md) "folder2"))

(define S15 ((run S14 md) "folder2"))
(define S16 ((run S15 md) "folder3"))

;ingresa a carpeta folder2
(define S17 ((run S16 cd) "folder2"))

;crea subcarpeta folder21 dentro de folder2 (incluye caso S19 de carpeta con nombre duplicado)
(define S18 ((run S17 md) "folder21"))
(define S19 ((run S18 md) "folder21"))

;ingresa a subcarpeta e intenta ingresar a subcarpeta inexistente S221
(define S20 ((run S19 cd) "folder21"))
(define S21 ((run S20 cd) "folder22"))

;vuelve a carpeta anterior
(define S22 ((run S21 cd) ".."))

;vuelve a ingresar folder21
(define S23 ((run S22 cd) "folder21"))

;crea subcarpeta folder211 e ingresa
(define S24 ((run S23 md) "folder211"))
(define S25 ((run S24 cd) "folder211"))

;vuelve a la raíz de la unidad c:/
(define S26 ((run S24 cd) "/"))

;se cambia de unidad
(define S27 ((run S26 switch-drive) #\D))

;crea carpeta e ingresa a carpeta
(define S28 ((run S27 md) "folder5"))
(define S29 ((run S28 cd) "folder5"))

;se cambia de carpeta en base a la ruta especificada
(define S30 ((run S29 cd) "C:/folder1/"))

;formateando drive D:
(define S31 ((run S30 format) #\D "newD"))

;añadiendo archivos
(define S32 ((run S31 add-file) (file "foo1.txt" "txt" "hello world 1")))
(define S33 ((run S32 add-file) (file "foo2.txt" "txt" "hello world 2")))
(define S34 ((run S33 add-file) (file "foo3.docx" "docx" "hello world 3")))
(define S35 ((run S34 add-file) (file "goo4.docx" "docx" "hello world 4" #\h #\r))) ;con atributos de seguridad oculto (h) y de solo lectura (r)

;eliminando archivos
;;(define S36 ((run S35 del) "*.txt"))
;;(define S37 ((run S35 del) "f*.docx"))
(define S38 ((run S35 del) "goo4.docx"))
(define S39 ((run S35 cd) ".."))
(define S40 ((run S39 del) "folder1"))

;borrando una carpeta
(define S41 ((run S39 rd) "folder1"))  ;no debería borrarla, pues tiene archivos
(define S42 ((run S41 cd) "folder1"))

;MODIFIQUE ESTO PARA QUE SIGA CORRIENDO EL PROGRAMA PERO
;EN LA AUTOEVALUACION CONSIDERE LA IMPLEMENTACION AL 50%
;(define S43 ((run S42 del) "*.*"))
(define S43.0 ((run S42 del) "goo4.docx")) ;;
(define S43.1 ((run S43.0 del) "foo3.docx")) ;;
(define S43.2 ((run S43.1 del) "foo2.txt")) ;;
(define S43 ((run S43.2 del) "foo1.txt")) ;;

(define S44 ((run S43 cd) "..")) ;((run S43 cd) ".."))
(define S45 ((run S44 rd) "folder1"))

;copiando carpetas y archivos
(define S46 ((run S35 copy) "foo1.txt" "c:/folder3/"))
(define S47 ((run S46 cd) ".."))
(define S48 ((run S47 copy) "folder1" "d:/"))

;moviendo carpetas y archivos
(define S49 ((run S48 move) "folder3" "d:/"))
(define S50 ((run S49 cd) "folder1"))
(define S51 ((run S50 move) "foo3.docx" "d:/folder3/"))

;renombrando carpetas y archivos
;(define S52 ((run S51 ren) "foo1.txt" "newFoo1.txt"))
;(define S53 ((run S52 ren) "foo2.txt" "newFoo1.txt")) ;no debería efectuar cambios pues ya existe archivo con este nombre
;(define S54 ((run S53 cd) ".."))
;(define S55 ((run S54 ren) "folder1" "newFolder1"))

;listando la información
;(display (run S16 dir))
;(display (run S55 dir))
;(display ((run S55 dir) "/s")) ;muestra carpetas y subcarpetas de la unidad C
;(display ((run S55 dir) "/s /a")) ;muestra todo el contenido de carpetas y subcarpetas de la unidad C incluyendo archivo oculto goo4.docx

;encriptando archivos y carpetas
;(define S56 ((run S55 encrypt) plus-one minus-one "1234" "newFolder1"))
;(define S57 ((run S56 switch-drive) #\D))
;(define S58 ((run S57 cd) "folder3"))
;(define S59 ((run S58 encrypt) plus-one minus-one "4321" "foo3.docx"))

;desencriptando archivos y carpetas
;(define S60 ((run S59 decrypt) "1234" "foo3.docx")) ;no logra desencriptar por clave incorrecta
;(define S61 ((run S60 decrypt) "4321" "foo3.docx"))
;(define S62 ((run S61 switch-drive) #\C))
;(define S63 ((run S62 decrypt) "1234" "newFolder1"))

;;buscando contenido
;(define S64 ((run S63 cd) "newFolder1"))
;(display ((run S64 grep) "hello" "newFoo1.txt"))
;(display ((run S64 grep) "hello" "*.*"))

;viendo la papelera
;(display (run S45 viewTrash))

;restaurando
;(define S65 ((run S45 restore) "folder1"))

(display "\nSCRIPT DE PRUEBAS PERSONAL (3 ej por funcion implementada)\n
          Funciones no implementadas:\n
          - ren
          - dir
          - format(implementada sin prerequisito)
          - encrypt
          - decrypt
          - plus-one
          - minus-one
          - grep
          - view-trash
          - restore")

(define A0 (system "newOS"))

;añadiendo unidades. Incluye caso A2 que intenta añadir unidad con una letra que ya existe
(define A1 ((run A0 add-drive) #\D "Drive1" 1000))
(define A2 ((run A1 add-drive) #\D "Drive1.1" 3000))
(define A3 ((run A2 add-drive) #\C "Drive2" 2000))

;añadiendo usuarios. Incluye caso A6 que intenta registrar usuario duplicado
(define A4 ((run A3 register) "user1"))
(define A5 ((run A4 register) "user2"))
(define A6 ((run A5 register) "user2"))

;iniciando sesión con usuarios. Incluye caso A8 que intenta iniciar sesión con user2 sin antes haber salido con user1
(define A7 ((run A6 login) "user1"))
(define A8 ((run A7 login) "user2"))

;cerrando sesión user1 e iniciando con user2
;cerrando sesión user2 e iniciando con user1
(define A9 (run A8 logout))
(define A10 ((run A9 login) "user2"))
(define A10.1 (run A10 logout))
(define A10.2 ((run A10.1 login) "user1"))

;cambios de unidad, incluyendo unidad inexistente E
(define A11 ((run A10 switch-drive) #\E))
(define A11.1 ((run A11 switch-drive) #\C))
(define A12 ((run A11.1 switch-drive) #\D))

;añadiendo carpetas. Incluye casos de carpetas duplicadas.
(define A13 ((run A12 md) "carpeta1"))
(define A14 ((run A13 md) "carpeta1"))
(define A15 ((run A14 md) "carpeta2"))
(define A16 ((run A15 md) "carpeta3"))

;ingresa a carpeta carpeta2
(define A17 ((run A16 cd) "carpeta3"))

;crea subcarpeta carpeta31 dentro de carpeta3
;(incluye caso A19 de carpeta con nombre duplicado)
(define A18 ((run A17 md) "carpeta31"))
(define A19 ((run A18 md) "carpeta31"))

;ingresa a subcarpeta e intenta ingresar a subcarpeta inexistente 311
(define A20 ((run A19 cd) "carpeta31"))
(define A21 ((run A20 cd) "carpeta311"))

;vuelve a carpeta anterior
(define A22 ((run A21 cd) ".."))

;vuelve a ingresar carpeta31
(define A23 ((run A22 cd) "carpeta31"))

;crea subcarpeta carpeta211 e ingresa
(define A24 ((run A23 md) "carpeta311"))
(define A25 ((run A24 cd) "carpeta311"))

;vuelve a la raíz de la unidad
(define A26 ((run A24 cd) "/"))

;se cambia de unidad
(define A27 ((run A26 switch-drive) #\C))

;crea carpeta e ingresa a carpeta
(define A28 ((run A27 md) "carpeta5"))
(define A29 ((run A28 cd) "carpeta5"))

;se cambia de carpeta en base a la ruta especificada
(define A30 ((run A29 cd) "D:/carpeta1/"))

;formateando drive D:
(define A31 ((run A30 format) #\D "newDriveD"))

;añadiendo archivos
(define A32 ((run A31 cd) "C:/carpeta5/"))
(define A33 ((run A32 add-file) (file "file1.txt" "txt" "hello world 2")))
(define A34 ((run A33 add-file) (file "file2.docx" "docx" "hello world 3")))
(define A34.1 ((run A34 add-file) (file "file22.docx" "docx" "hello world 333")))
(define A35 ((run A34.1 add-file) (file "file3.txt" "txt" "hello world 4" #\h #\r))) ;con atributos de seguridad oculto (h) y de solo lectura (r)

(define A36 ((run A35 cd) ".."))
(define A36.1 ((run A36 md) "carpeta6"))
(define A36.2 ((run A36.1 cd) "carpeta6"))
(define A37 ((run A36.2 add-file) (file "file4.docx" "docx" "hello world 4")))
(define A37.1 ((run A37 cd) "C:/carpeta5/"))

;eliminando archivos

(define A38 ((run A37.1 del) "file2.docx"))
(define A39 ((run A38 del) "file1.txt"))
(define A40 ((run A39 del) "file3.txt"))

;borrando una carpeta
(define A41 ((run A37.1 cd) "/"))
(define A42 ((run A41 rd) "carpeta6")) ;no debería borrarla, pues tiene archivos
(define A43 ((run A42 cd) "carpeta6"))
(define A44 ((run A43 del) "file4.docx"))
(define A45 ((run A44 cd) ".."))
(define A46 ((run A45 rd) "carpeta6"));la borra porque esta vacia

;crea nueva carpeta
(define A46.1 ((run A43 cd) "/"))
(define A47 ((run A46.1 md) "carpeta7"))

;copiando carpetas y archivos
(define A48 ((run A47 copy) "carpeta5" "c:/carpeta7/"))
(define A49 ((run A48 cd) "C:/"))
(define A50 ((run A49 copy) "carpeta6" "d:/"))
(define A51 ((run A50 copy) "carpeta3" "d:/"));copiar elemento inexistente

;moviendo carpetas y archivos
(define A52 ((run A49 move) "carpeta3" "d:/"));mover elemento inexistente
(define A52.1 ((run A52 move) "carpeta6" "d:/"))
(define A53 ((run A52.1 switch-drive) #\D))
(define A54 ((run A53 move) "carpeta6" "C:/"));mueve de vuelta el contenido

(define A55 ((run A54 format) #\C "NewC"))
