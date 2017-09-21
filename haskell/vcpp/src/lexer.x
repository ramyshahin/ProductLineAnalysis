-- lexer.x 
--  lexical structure of CPP files
--  this is an alex file
{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z_]

tokens :-

    $white+             ;

    -- preprocessor directives
    \#include           {\s -> P_INCLUDE}
    \#define            {\s -> P_DEFINE}
    \#undef             {\s -> P_UNDEF}
    \#ifdef             {\s -> P_IFDEF}
    \#if                {\s -> P_IF}
    \#else              {\s -> P_ELSE}
    \#endif             {\s -> P_ENDIF}
    \#error             {\s -> P_ERROR}
    \#warning           {\s -> P_WARNING}

    --keywords
    auto                {\s -> AUTO} 
    break               {\s -> BREAK}
    case                {\s -> CASE}
    char                {\s -> CHAR}
    const               {\s -> CONST} 
    continue            {\s -> CONTINUE} 
    default             {\s -> DEFAULT} 
    do                  {\s -> DO} 
    double              {\s -> DOUBLE}
    else                {\s -> ELSE}
    enum                {\s -> ENUM} 
    extern              {\s -> EXTERN} 
    float               {\s -> FLOAT} 
    for                 {\s -> FOR} 
    goto                {\s -> GOTO} 
    if                  {\s -> IF}
    int                 {\s -> INT} 
    long                {\s -> LONG}
    register            {\s -> REGISTER} 
    return              {\s -> RETURN} 
    short               {\s -> SHORT}
    signed              {\s -> SIGNED} 
    sizeof              {\s -> SIZEOF}
    static              {\s -> STATIC}
    struct              {\s -> STRUCT} 
    switch              {\s -> SWITCH} 
    typedef             {\s -> TYPEDEF} 
    union               {\s -> UNION} 
    unsigned            {\s -> UNSIGNED} 
    void                {\s -> VOID} 
    volatile            {\s -> VOLATILE} 
    while               {\s -> WHILE}

    -- identifiers
    $alpha [$alpha $digit]* {\s -> ID s}

    -- symbols
    [\=\+\-\*\/\(\)\{\}\[\]\"\'\!\?\.\,\^\%~:\;\<\>] {\s -> SYM (head s)} 

{
data Token =
    -- preprocessor directives
    P_INCLUDE   |
    P_DEFINE    |
    P_UNDEF     |
    P_IFDEF     |
    P_IF        |
    P_ELSE      |
    P_ENDIF     |
    P_ERROR     |
    P_WARNING   |

    -- keywords
    AUTO        | 
    BREAK       |
    CASE        |
    CHAR        |
    CONST       | 
    CONTINUE    | 
    DEFAULT     | 
    DO          | 
    DOUBLE      |
    ELSE        |
    ENUM        | 
    EXTERN      | 
    FLOAT       | 
    FOR         | 
    GOTO        | 
    IF          |
    INT         | 
    LONG        |
    REGISTER    | 
    RETURN      | 
    SHORT       |
    SIGNED      | 
    SIZEOF      |
    STATIC      |
    STRUCT      | 
    SWITCH      | 
    TYPEDEF     | 
    UNION       | 
    UNSIGNED    | 
    VOID        | 
    VOLATILE    | 
    WHILE       |

    -- identifiers
    ID String   |
    SYM Char
    deriving (Eq, Show)

--main = do
--    s <- getContents
--    alexScanTokens s
}