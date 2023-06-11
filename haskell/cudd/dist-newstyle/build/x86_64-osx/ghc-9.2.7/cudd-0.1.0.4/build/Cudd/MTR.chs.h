#define C2HS_MIN_VERSION(mj,mn,rv) ((mj)<=C2HS_VERSION_MAJOR && (mn)<=C2HS_VERSION_MINOR && (rv)<=C2HS_VERSION_REV)
#include <stdio.h>
#include <mtr.h>
enum MTR_TYPES {
    MTRDefault = MTR_DEFAULT,
    MTRTerminal = MTR_TERMINAL,
    MTRSoft = MTR_SOFT,
    MTRFixed = MTR_FIXED,
    MTRNewNode = MTR_NEWNODE
};


