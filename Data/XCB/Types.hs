{-# LANGUAGE DeriveDataTypeable #-}

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


module Data.XCB.Types where


import qualified Data.List as L
import Control.Monad

import Data.Generics (Data) -- base 3
import Data.Typeable

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
 deriving (Show, Data, Typeable)

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
 deriving (Show, Data, Typeable)

data GenStructElem typ
    = Pad Int
    | List Name typ (Maybe Expression) (Maybe (EnumVals typ))
    | SField Name typ (Maybe (EnumVals typ)) (Maybe (MaskVals typ))
    | ExprField Name typ Expression
    | ValueParam typ MaskName (Maybe MaskPadding) ListName
 deriving (Show, Data, Typeable)

type AltEnumVals typ = typ
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
 deriving (Show, Data, Typeable)

-- Should only ever have expressions of type 'Value' or 'Bit'.
data EnumElem = EnumElem Name (Maybe Expression)
 deriving (Show, Data, Typeable)

-- |Declarations may contain expressions from this small language
data Expression = Value Int  -- ^A literal value
                | Bit Int    -- ^A log-base-2 literal value
                | FieldRef String -- ^A reference to a field in the same declaration
                | Op Binop Expression Expression -- ^A binary opeation
 deriving (Show, Data, Typeable)

-- |Supported Binary operations.
data Binop = Add
           | Sub
           | Mult
           | Div
           | And
           | RShift
 deriving (Show, Data, Typeable)

