{-# LANGUAGE
     RecordWildCards
     #-}

-- |
-- Module    :  Data.XCB.Types
-- Copyright :  (c) Antoine Latter 2008
-- License   :  BSD3
--
-- Maintainer:  Antoine Latter <aslatter@gmail.com>
-- Stability :  provisional
-- Portability: portable
--
-- Defines types inteneded to be equivalent to the schema used by
-- the XCB project in their XML protocol description.
--


module Data.XCB.Types
    ( XHeader
    , XDecl
    , StructElem
    , XidUnionElem
    , XReply
    , GenXHeader ( .. )
    , GenXDecl ( .. )
    , GenStructElem ( .. )
    , GenXReply
    , GenXidUnionElem ( .. )
    , EnumElem ( .. )
    , Expression ( .. )
    , Binop ( .. )
    , Type ( .. )
    , EnumVals
    , MaskVals
    , Name
    , Ref
    , MaskName
    , ListName
    , MaskPadding
    ) where


import Control.Monad

-- 'xheader_header' is the name gauranteed to exist, and is used in
-- imports and in type qualifiers.
--
-- 'xheader_name' is the InterCaps name, and should be prefered in the naming
-- of types, functions and haskell modules when available.
-- |This is what a single XML file maps to.  It contains some meta-data
-- then declarations.
data GenXHeader typ = XHeader
    {xheader_header :: Name -- ^Name of module.  Used in the other modules as a reference.
    ,xheader_xname :: Maybe Name  -- ^Name used to indentify extensions between the X client and server.
    ,xheader_name :: Maybe Name -- ^InterCaps name.
    ,xheader_multiword :: Maybe Bool
    ,xheader_major_version :: Maybe Int
    ,xheader_minor_version :: Maybe Int
    ,xheader_decls :: [GenXDecl typ]  -- ^Declarations contained in this module.
    }
 deriving (Show)

instance Functor GenXHeader where
    fmap = mapTypes

mapTypes :: (a -> b) -> GenXHeader a -> GenXHeader b
mapTypes f XHeader{..} =
    XHeader
     xheader_header
     xheader_xname
     xheader_name
     xheader_multiword
     xheader_major_version
     xheader_minor_version
     (map (mapDecls f) xheader_decls)

type XHeader = GenXHeader Type
type XDecl = GenXDecl Type
type StructElem = GenStructElem Type
type XidUnionElem = GenXidUnionElem Type
type XReply = GenXReply Type

-- |The different types of declarations which can be made in one of the
-- XML files.
data GenXDecl typ
    = XStruct  Name [GenStructElem typ]
    | XTypeDef Name typ
    | XEvent Name Int [GenStructElem typ] (Maybe Bool)  -- ^ The boolean indicates if the event includes a sequence number.
    | XRequest Name Int [GenStructElem typ] (Maybe (GenXReply typ))
    | XidType  Name
    | XidUnion  Name [GenXidUnionElem typ]
    | XEnum Name [EnumElem]
    | XUnion Name [GenStructElem typ]
    | XImport Name
    | XError Name Int [GenStructElem typ]
 deriving (Show)

instance Functor GenXDecl where
    fmap = mapDecls

mapDecls :: (a -> b) -> GenXDecl a -> GenXDecl b
mapDecls f = go
 where
   go (XStruct name elems) = XStruct name (map (mapSElem f) elems)
   go (XTypeDef name t) = XTypeDef name (f t)
   go (XEvent name n elems seqNum)
       = XEvent name n (map (mapSElem f) elems) seqNum
   go (XRequest name n elems rep) = XRequest name n (map (mapSElem f) elems) (mapReply f rep)
   go (XidType name) = XidType name
   go (XEnum name elems) = XEnum name elems
   go (XUnion name elems) = XUnion name (map (mapSElem f) elems)
   go (XidUnion name elems) = XidUnion name (map (mapUnions f) elems)
   go (XImport name) = XImport name
   go (XError name n elems) = XError name n (map (mapSElem f) elems)

mapReply :: Functor f =>
            (typ -> typ') -> f [GenStructElem typ] -> f [GenStructElem typ']
mapReply f = fmap (map (mapSElem f))

data GenStructElem typ
    = Pad Int
    | List Name typ (Maybe Expression) (Maybe (EnumVals typ))
    | SField Name typ (Maybe (EnumVals typ)) (Maybe (MaskVals typ))
    | ExprField Name typ Expression
    | ValueParam typ Name (Maybe MaskPadding) ListName
 deriving (Show)

instance Functor GenStructElem where
    fmap = mapSElem

mapSElem :: (typ -> typ') -> GenStructElem typ -> GenStructElem typ'
mapSElem f = go
 where
   go (Pad n) = Pad n
   go (List name typ expr enum) = List name (f typ) expr (liftM f enum)
   go (SField name typ enum mask) = SField name (f typ) (liftM f enum) (liftM f mask)
   go (ExprField name typ expr) = ExprField name (f typ) expr
   go (ValueParam typ name pad lname) = ValueParam (f typ) name pad lname

type EnumVals typ = typ
type MaskVals typ = typ

type Name = String
type GenXReply typ = [GenStructElem typ]
type Ref = String
type MaskName = Name
type ListName = Name
type MaskPadding = Int

-- |Types may include a reference to the containing module.
data Type = UnQualType Name
          | QualType Name Name
 deriving Show

data GenXidUnionElem typ = XidUnionElem typ
 deriving (Show)

instance Functor GenXidUnionElem where
    fmap = mapUnions

mapUnions :: (typ -> typ') -> GenXidUnionElem typ -> GenXidUnionElem typ'
mapUnions f (XidUnionElem t) = XidUnionElem (f t)

-- Should only ever have expressions of type 'Value' or 'Bit'.
data EnumElem = EnumElem Name (Maybe Expression)
 deriving (Show)

-- |Declarations may contain expressions from this small language
data Expression = Value Int  -- ^A literal value
                | Bit Int    -- ^A log-base-2 literal value
                | FieldRef Name -- ^A reference to a field in the same declaration
                | Op Binop Expression Expression -- ^A binary opeation
 deriving (Show)

-- |Supported Binary operations.
data Binop = Add
           | Sub
           | Mult
           | Div
           | And
           | RShift
 deriving (Show)

