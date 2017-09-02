#getSampleSizeMatrix
getSampleSizeMatrix('z', c(0.5))
getSampleSizeMatrix('t', c(5,1), delta_arr = c(0.01,0.02,0.03,0.04),samplePerc_arr = c(0.1,0.2,0.3,0.4))

#getSampleSizeMatrices
getSampleSizeMatrices('z', list(c(0.5), c(0.6), c(0.8)))
getSampleSizeMatrices('t', list(c(5,1), c(7,3.2), c(8,2.6)),delta_arr = c(0.01,0.02,0.03,0.04),samplePerc_arr = c(0.1,0.2,0.3,0.4), power = 0.9, sig.level = 0.01)


