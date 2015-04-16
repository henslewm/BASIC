   Private Function sSpeedCorr(ByVal tubeNum As Integer) As Single
        ' this is the distance to be added to the controller out to compensate from a temperature gradient
        Dim calHole As Single = 1.12  ' meters from ref    
        Dim tdSum, tCalAvg, tTotalAvg, lastPoint, thisPoint, thisTemp, lastTemp, echoPoint As Single, i As Integer
        Dim tubeLenChange As Single
        Const Kelvin As Single = 273.15  ' 0 deg C = 273.15 K

        ' compute cal tube avg t
        echoPoint = calHole + toolOffset
        tdSum = 0
        i = 0
        lastPoint = -0.1 'meters this is where the transducer is relative to the ref
        lastTemp = tubeTemp(thSeq(i))
        Do While therm(thSeq(i)).disToRef < echoPoint And i < maxSeq
            thisPoint = therm(thSeq(i)).disToRef
            thisTemp = tubeTemp(thSeq(i))  'therm(seq(i)).temp
            tdSum += (thisPoint - lastPoint) * (thisTemp + lastTemp) / 2
            lastTemp = thisTemp
            lastPoint = thisPoint
            i += 1
        Loop
        tdSum += (echoPoint - lastPoint) * lastTemp
        tCalAvg = tdSum / (echoPoint + 0.1) + Kelvin
        calTubeTemp = tCalAvg - Kelvin

        'compute total sound tube avg
        echoPoint = tape(tubeNum).length
        tdSum = 0
        i = 0
        lastPoint = -0.1 'meters this is where the transducer is relative to the ref
        lastTemp = tubeTemp(thSeq(i))
        Do While therm(thSeq(i)).disToRef < echoPoint And i < maxSeq
            thisPoint = therm(thSeq(i)).disToRef
            thisTemp = tubeTemp(thSeq(i))
            tdSum += (thisPoint - lastPoint) * (thisTemp + lastTemp) / 2
            lastTemp = thisTemp
            lastPoint = thisPoint
            i += 1
        Loop
        tdSum += (echoPoint - lastPoint) * lastTemp
        tTotalAvg = tdSum / (echoPoint + 0.1) + Kelvin
        ' the cpvc tube expands with temperature, it is added as part of the sound speed correction
        tubeLenChange = (tTotalAvg - Kelvin - cpvcRefTemp) * cpvcTC * tape(tubeNum).length

        sSpeedCorr = (tape(tubeNum).length + 0.1) * (Sqrt(tTotalAvg / tCalAvg) - 1) + tubeLenChange
        'Debug.WriteLine(tCalAvg.ToString & " " & tTotalAvg.ToString & " ssc " & sSpeedCorr.ToString & "  " & tubeLenChange.ToString)


    End Function