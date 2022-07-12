import kmedianas._


val numPuntos = 10
val eta = 0.01
val k = 32
val puntosSeq = generarPuntosSeq (k, numPuntos)
val medianasSeq = inicializarMedianasSeq(k, puntosSeq)

clasificarSeq(puntosSeq, medianasSeq)
kmedianasSeq(puntosSeq, medianasSeq, eta)

println("hola")
