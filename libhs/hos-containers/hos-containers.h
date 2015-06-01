
#define STRICT_1_OF_2(fn) fn arg _ | arg `seq` False = undefined
#define STRICT_2_OF_2(fn) fn _ arg | arg `seq` False = undefined
#define STRICT_1_OF_3(fn) fn arg _ _ | arg `seq` False = undefined
#define STRICT_2_OF_3(fn) fn _ arg _ | arg `seq` False = undefined
#define STRICT_1_OF_4(fn) fn arg _ _ _ | arg `seq` False = undefined
#define STRICT_2_OF_4(fn) fn _ arg _ _ | arg `seq` False = undefined
