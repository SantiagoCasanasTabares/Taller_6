import kmedianas._


val numPuntos = 1
val eta = 0.01
val k = 2
val puntosSeq = generarPuntosSeq (k, numPuntos)
val medianasSeq = inicializarMedianasSeq(k, puntosSeq)
clasificarSeq(puntosSeq, medianasSeq)
kmedianasSeq(puntosSeq, medianasSeq, eta)
println("hola")
