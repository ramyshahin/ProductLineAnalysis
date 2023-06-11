#define C2HS_MIN_VERSION(mj,mn,rv) ((mj)<=C2HS_VERSION_MAJOR && (mn)<=C2HS_VERSION_MINOR && (rv)<=C2HS_VERSION_REV)
#include "dddmp.h"
enum DddmpMode {
    DddmpModeText    = 65,
    DddmpModeBinary  = 66,
    DddmpModeDefault = 68
};



