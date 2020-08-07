module Test.SignalProcessing where

import Prelude (Unit, discard, negate)
import Data.Teller.SignalProcessing (estimatePeriod, autoCorrelation, averageDistance, chunkConsec, diffChunks, findPeaks, naiveSignalMatch)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


signalProcessingSpec :: Spec Unit
signalProcessingSpec =
    describe "Signal Processing" do 
        describe "autoCorrelation" do
            it "calculates the correct autocorrelation for a simple series" do
                let res = autoCorrelation [1.0,2.0,1.0,2.0,1.0,2.0,1.0,2.0]
                res `shouldEqual` [1.0,-0.875,0.75,-0.625,0.5,-0.375,0.25,-0.125]
            it "calculates the correct autocorrelation for a more complex series" do
                let res = autoCorrelation [
                    1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                    1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                    1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                    1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                    1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                    1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                    1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                    1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                    1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                    1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                    1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                    1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 
                    1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0
                    ]
                res `shouldEqual` [1.0,0.3109890109890117,-0.3780219780219779,-0.38241758241758234,-0.3868131868131867,-0.3912087912087911,0.2659340659340665,0.923076923076923,0.28791208791208855,-0.3472527472527472,-0.3516483516483516,-0.356043956043956,-0.3604395604395604,0.24285714285714333,0.846153846153846,0.2648351648351654,-0.3164835164835164,-0.3208791208791209,-0.3252747252747252,-0.3296703296703296,0.21978021978022014,0.7692307692307692,0.24175824175824223,-0.2857142857142857,-0.29010989010989013,-0.29450549450549446,-0.2989010989010989,0.19670329670329698,0.6923076923076925,0.21868131868131904,-0.254945054945055,-0.25934065934065936,-0.2637362637362637,-0.2681318681318681,0.17362637362637387,0.6153846153846158,0.1956043956043959,-0.2241758241758242,-0.22857142857142865,-0.23296703296703297,-0.23736263736263738,0.15054945054945074,0.538461538461539,0.1725274725274728,-0.19340659340659347,-0.19780219780219788,-0.20219780219780223,-0.20659340659340664,0.12747252747252763,0.4615384615384623,0.14945054945054967,-0.16263736263736273,-0.16703296703296713,-0.1714285714285715,-0.17582417582417592,0.10439560439560454,0.3846153846153852,0.12637362637362656,-0.13186813186813198,-0.1362637362637364,-0.14065934065934074,-0.14505494505494515,0.08131868131868142,0.30769230769230815,0.10329670329670344,-0.10109890109890121,-0.1054945054945056,-0.10989010989011,-0.11428571428571441,0.058241758241758305,0.2307692307692311,0.08021978021978032,-0.07032967032967041,-0.07472527472527482,-0.07912087912087921,-0.08351648351648362,0.035164835164835206,0.15384615384615402,0.057142857142857204,-0.03956043956043961,-0.04395604395604401,-0.048351648351648416,-0.05274725274725282,0.0120879120879121,0.07692307692307701,0.034065934065934105,-0.008791208791208802,-0.013186813186813204,-0.017582417582417603,-0.021978021978022004,-0.010989010989011002]
        describe "findPeaks" do
            it "finds peaks in a simple series" do
                let res = findPeaks [0.0, 1.0,-0.875,0.75,-0.625,0.5,-0.375,0.25,-0.125] 1 1.0
                res `shouldEqual` [1, 3]
            it "finds peaks in a more complex series" do 
                let res = findPeaks [1.0,0.3109890109890117,-0.3780219780219779,-0.38241758241758234,-0.3868131868131867,-0.3912087912087911,0.2659340659340665,0.923076923076923,0.28791208791208855,-0.3472527472527472,-0.3516483516483516,-0.356043956043956,-0.3604395604395604,0.24285714285714333,0.846153846153846,0.2648351648351654,-0.3164835164835164,-0.3208791208791209,-0.3252747252747252,-0.3296703296703296,0.21978021978022014,0.7692307692307692,0.24175824175824223,-0.2857142857142857,-0.29010989010989013,-0.29450549450549446,-0.2989010989010989,0.19670329670329698,0.6923076923076925,0.21868131868131904,-0.254945054945055,-0.25934065934065936,-0.2637362637362637,-0.2681318681318681,0.17362637362637387,0.6153846153846158,0.1956043956043959,-0.2241758241758242,-0.22857142857142865,-0.23296703296703297,-0.23736263736263738,0.15054945054945074,0.538461538461539,0.1725274725274728,-0.19340659340659347,-0.19780219780219788,-0.20219780219780223,-0.20659340659340664,0.12747252747252763,0.4615384615384623,0.14945054945054967,-0.16263736263736273,-0.16703296703296713,-0.1714285714285715,-0.17582417582417592,0.10439560439560454,0.3846153846153852,0.12637362637362656,-0.13186813186813198,-0.1362637362637364,-0.14065934065934074,-0.14505494505494515,0.08131868131868142,0.30769230769230815,0.10329670329670344,-0.10109890109890121,-0.1054945054945056,-0.10989010989011,-0.11428571428571441,0.058241758241758305,0.2307692307692311,0.08021978021978032,-0.07032967032967041,-0.07472527472527482,-0.07912087912087921,-0.08351648351648362,0.035164835164835206,0.15384615384615402,0.057142857142857204,-0.03956043956043961,-0.04395604395604401,-0.048351648351648416,-0.05274725274725282,0.0120879120879121,0.07692307692307701,0.034065934065934105,-0.008791208791208802,-0.013186813186813204,-0.017582417582417603,-0.021978021978022004,-0.010989010989011002] 1 1.0
                res `shouldEqual` [0, 7, 14, 21, 28, 35, 42, 49, 56, 63, 70]
        describe "chunkConsec" do
            it "chunks consecutive items" do
                let res = chunkConsec [1, 2, 3, 4, 5, 6, 7]
                res `shouldEqual` [[1, 2], [2, 3], [3, 4], [4, 5], [5,6], [6, 7]]
        describe "diffChunks" do
            it "diffs chunks correctly" do
                let res = diffChunks [[1.0, 5.0], [9.0,3.0], [0.0, 0.0]]
                res `shouldEqual` [4.0, 6.0, 0.0]
        describe "averageDistance" do
            it "returns the correct distance between peaks" do
                let res = averageDistance [0.0, 7.0, 14.0, 21.0, 28.0, 35.0, 42.0, 49.0, 56.0, 63.0, 70.0]
                res `shouldEqual` 7.0
        describe "estimatePeriod" do
            it "calculates period correctly" do
                let res = estimatePeriod [
                    1, 1, 1, 1, 1, 0, 0,
                    1, 1, 1, 1, 1, 0, 0,
                    1, 1, 1, 1, 1, 0, 0,
                    1, 1, 1, 1, 1, 0, 0,
                    1, 1, 1, 1, 1, 0, 0,
                    1, 1, 1, 1, 1, 0, 0,
                    1, 1, 1, 1, 1, 0, 0,
                    1, 1, 1, 1, 1, 0, 0
                    ]
                res `shouldEqual` 7.0
        describe "naiveSignalMatch" do
            it "returns correctly for a 0 length array" do
                let res = naiveSignalMatch [] []
                res `shouldEqual` 100.0
            it "returns correctly for a fully matching array" do
                let res = naiveSignalMatch [1, 3, 4, 9, 9, 10] [1, 3, 4, 9, 9, 10]
                res `shouldEqual` 100.0
            it "returns correctly for a partially matching array" do
                let res = naiveSignalMatch [1, 3, 4, 9, 9, 2, 11, 6, 9, 10] [1, 3, 4, 9, 9, 2, 11, 6, 9, 100]
                res `shouldEqual` 90.0
            it "returns correctly for a non matching array" do
                let res = naiveSignalMatch [1, 3, 4, 9, 9, 2, 11, 6, 9, 10] [5, 5, 5, 5, 5, 5, 5, 5, 5, 5]
                res `shouldEqual` 0.0
