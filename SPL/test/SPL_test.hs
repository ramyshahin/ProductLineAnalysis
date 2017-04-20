-- SPL unit tests
import SPL_prop
import SPL
import Shallow.VList
main = do 
    runSPLTests
    print sl1
    print sl2
    let l1 = vlength sl1
    let l2 = vlength sl2
    print $ l1
    print $ l2
    print $ l1 == l2
    print $ l1 `isSubsetOf` l2
    print $ l2 `isSubsetOf` l1
