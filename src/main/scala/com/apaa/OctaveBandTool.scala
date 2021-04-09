package com.apaa

import org.slf4j.LoggerFactory

case class FreqValue(freq: Float, db: Float)
object OctaveBandTool {
  val logger = LoggerFactory.getLogger(this.getClass)

  val ONE_THIRD_OCTAVE_BANDS_BOUND = Seq[Double](
    5.6, 7.1, 8.9, 11.2, 14.1,
    17.8, 22.4, 28.2, 35.5, 44.7, 56.2, 70.8,
    89.1, 112, 141, 178, 224, 282, 355, 447, 562, 708, 891, 1122, 1413,
    1778, 2239, 2818, 3548, 4467, 5623, 7079, 8913, 11220, 14130, 17780,
    22390) //37

  lazy val OneThirdOctaveBandRange: Seq[(Double, Double)] = {
    val left = ONE_THIRD_OCTAVE_BANDS_BOUND.dropRight(1)
    val right = ONE_THIRD_OCTAVE_BANDS_BOUND.drop(1)
    left.zip(right)
  }

  lazy val OneThirdOctaveBandRangeIdx = OneThirdOctaveBandRange.zipWithIndex
  logger.info(OneThirdOctaveBandRangeIdx.toString)

  lazy val OneThirdOctaveBand: Seq[Double] = Seq(
    6.3d,
    8d,
    10d,
    12.5d,
    16d,
    20d,
    25d,
    31.5d,
    40d,
    50d,
    63d,
    80d,
    100d,
    125d,
    160d,
    200d,
    250d,
    315d,
    400d,
    500d,
    630d,
    800d,
    1000d,
    1250d,
    1600d,
    2000d,
    2500d,
    3150d,
    4000d,
    5000d,
    6300d,
    8000d,
    10000d,
    12500d,
    16000d,
    20000d,
  )

  def getOneThirdOctaveBand(freq: Float) = {
    val ret = OneThirdOctaveBandRangeIdx.find(entry => {
      val ((lower, upper), idx) = entry
      freq >= lower && freq < upper
    })

    for(((_, _), idx) <- ret) yield
      OneThirdOctaveBand(idx)
  }

  def octaveBandAWeighting = Map[Double, Double](
    6.3d -> -85.4d,
    8d -> -77.8d,
    10d -> -70.4d,
    12.5d -> -63.4d,
    16d -> -56.7d,
    20d -> -50.5d,
    25d -> -44.7d,
    31.5d -> -39.4d,
    40d -> -34.6d,
    50d -> -30.2d,
    63d -> -26.2d,
    80d -> -22.5d,
    100d -> -19.1d,
    125d -> -16.1d,
    160d -> -13.4d,
    200d -> -10.9d,
    250d -> -8.6d,
    315d -> -6.6d,
    400d -> -4.8d,
    500d -> -3.2d,
    630d -> -1.9d,
    800d -> -0.8d,
    1000d -> 0d,
    1250d -> 0.6d,
    1600d -> 1d,
    2000d -> 1.2d,
    2500d -> 1.3d,
    3150d -> 1.2d,
    4000d -> 1d,
    5000d -> 0.5d,
    6300d -> -0.1d,
    8000d -> -1.1d,
    10000d -> -2.5d,
    12500d -> -4.3d,
    16000d -> -6.6d,
    20000d -> -9.3d,
  ) // 36


  def calculateAweight(spectrum: Seq[FreqValue]) = {
    var total = 0d
    for(fv <- spectrum){
      for(octaveBand <-getOneThirdOctaveBand(fv.freq)){
        val aWeight = fv.db + octaveBandAWeighting(octaveBand)
        total += math.pow(10, aWeight/10)
      }
    }
    math.log10(total) * 10
  }
}
