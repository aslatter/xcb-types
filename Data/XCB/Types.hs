
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

-- 'xheader_header' is the name gauranteed to exist, and is used in
-- imports and in type qualifiers.
--
-- 'xheader_name' is the InterCaps name, and should be prefered in the naming
-- of types, functions and haskell modules when available.
-- |This is what a single XML file maps to.  It contains some meta-data
-- then declarations.
data XHeader = XHeader {xheader_header :: Name -- ^Name of module.  Used in the other modules as a reference.
                       ,xheader_xname :: Maybe Name  -- ^Name used to indentify extensions between the X client and server.
                       ,xheader_name :: Maybe Name -- ^InterCaps name.
                       ,xheader_multiword :: Maybe Bool
                       ,xheader_major_version :: Maybe Int
                       ,xheader_minor_version :: Maybe Int
                       ,xheader_decls :: [XDecl]  -- ^Declarations contained in this module.
                       }
 deriving (Show)

-- |The different types of declarations which can be made in one of the
-- XML files.
data XDecl = XStruct  Name [StructElem]
           | XTypeDef Name Type
           | XEvent Name Int [StructElem] (Maybe Bool)  -- ^ The boolean indicates if the event includes a sequence number.
           | XRequest Name Int [StructElem] (Maybe XReply)
           | XidType  Name
           | XidUnion  Name [XidUnionElem]
           | XEnum Name [EnumElem]
           | XUnion Name [StructElem]
           | XImport Name
           | XError Name Int [StructElem]
 deriving (Show)

data StructElem = Pad Int
                | List Name Type (Maybe Expression) (Maybe EnumVals)
                | SField Name Type (Maybe EnumVals) (Maybe MaskVals)
                | ExprField Name Type Expression
                | ValueParam Type MaskName (Maybe MaskPadding) ListName
 deriving (Show)

type AltEnumVals = Type
type EnumVals = Type
type MaskVals = Type

type Name = String
type XReply = [StructElem]
type Ref = String
type MaskName = Name
type ListName = Name
type MaskPadding = Int

-- |Types may include a reference to the containing module.
data Type = UnQualType Name
          | QualType Name Name
 deriving Show

data XidUnionElem = XidUnionElem Type
 deriving (Show)

-- Should only ever have expressions of type 'Value' or 'Bit'.
data EnumElem = EnumElem Name (Maybe Expression)
 deriving (Show)

-- |Declarations may contain expressions from this small language
data Expression = Value Int  -- ^A literal value
                | Bit Int    -- ^A log-base-2 literal value
                | FieldRef String -- ^A reference to a field in the same declaration
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

