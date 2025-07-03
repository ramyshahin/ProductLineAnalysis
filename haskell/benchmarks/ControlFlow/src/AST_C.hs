module AST_C where

{-
data CStructTag =
    CStructTag	 
  | CUnionTag

data CFunctionSpecifier =
    CInlineQual	 
  | CNoreturnQual

data CStorageSpecifier =
    CAuto
  | CRegister
  | CStatic
  | CExtern
  | CTypedef
  | CThread
  | CClKernel
  | CClGlobal
  | CClLocal

data CAttribute =
    CAttr Ident [CExpression]

data CTypeQualifier =
    CConstQual	 
  | CVolatQual	 
  | CRestrQual	 
  | CAtomicQual	 
  | CAttrQual CAttribute	 
  | CNullableQual
  | CNonnullQual	 
  | CClRdOnlyQual	 
  | CClWrOnlyQual

data CTypeSpecifier =
    CVoidType
  | CCharType
  | CShortType
  | CIntType
  | CLongType
  | CFloatType
  | CDoubleType
  | CSignedType
  | CUnsigType
  | CBoolType
  | CComplexType
  | CInt128Type
  | CUInt128Type
  | CBFloat16Type
  | CFloatNType Int Bool
  | CSUType CStructureUnion
  | CEnumType CEnumeration
  | CTypeDef Ident
  | CTypeOfExpr CExpression
  | CTypeOfType CDeclaration
  | CAtomicType CDeclaration

data CStructureUnion =
    CStruct CStructTag (Maybe Ident) [CDeclaration] [CAttribute]

data CEnumeration =
    CEnum (Maybe Ident) [(Ident, Maybe (CExpression a))] [CAttribute] 

data CDeclarationSpecifier =
    CStorageSpec CStorageSpecifier
  | CTypeSpec CTypeSpecifier	
  | CTypeQual CTypeQualifier
  | CFunSpec CFunctionSpecifier	
  | CAlignSpec CAlignmentSpecifier

data CAlignmentSpecifier =
    CAlignAsType CDeclaration
  | CAlignAsExpr CExpression

data CDeclaration =
    CDecl [CDeclarationSpecifier] [Maybe CDeclarator, Maybe CInitializer, Maybe CExpression]	 
  | CStaticAssert CExpression CStringLiteral


data CFunctionDef =
    CFunDef [CDeclarationSpecifier] CDeclarator CDeclaration CStatement

data CExternalDeclaration = 
    CDeclExt CDeclaration
  | CFDefExt CFunctionDef
  | CAsmExt  CStringLiteral

data CTranslationUnit = 
    CTranslUnit [CExternalDeclaration]

data CDeclarator =
    CDeclr (Maybe Ident) [CDerivedDeclarator] (Maybe CStringLiteral) [CAttribute]

data CDerivedDeclarator =
    CPtrDeclr [CTypeQualifier]
  | CArrDeclr [CTypeQualifier] CArraySize
  | CFunDeclr (Either [Ident] ([CDeclaration], Bool)) [CAttribute]

data CArraySize =
    CNoArrSize Bool
  | CArrSize Bool CExpression
-}