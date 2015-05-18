{-# LANGUAGE OverloadedStrings #-}
module ReWire.RouteLogic.Types where

import Data.Aeson
import Data.Aeson.Types
import Data.Data
import Data.Typeable
import Data.Monoid
import Control.Monad
import Control.Applicative
import Data.Attoparsec.Combinator
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V

--For translating inwards from JSON
data LinkNode = L Link | N NodeId NodeRef deriving Show

newtype LinkNodes = LNS [LinkNode] deriving Show
--getNodes :: LinkNodes -> [LinkNode]
--getNodes (LNS s) = s

type Text = T.Text
type Map  = M.Map

stripQuotes = T.filter (\x -> x == ' ')

type AnchorName = Text
type NodeRef = Text 

--type ArgMap = Map (NodeRef,AnchorName) Int
data NodeId  = Id Text | Input | Output deriving (Eq,Ord)
data Node    =   Device  NodeId NodeRef --Device Name, (clocked)
               | PureFun NodeId NodeRef Int deriving (Show,Eq,Ord)--Function Name, Arity


isPureFun x = case x of 
                PureFun _ _ _ -> True
                _ -> False
isDev x = case x of 
                Device _ _ -> True
                _ -> False

nodeId n = case n of
              Device i _ -> i
              PureFun i _ _ -> i

nodeRef n = case n of 
              Device _ i -> i
              PureFun _ i _ -> i

data IAnchor = IAnchor NodeId AnchorName deriving (Eq,Ord) --Source going to the input of a device/function "arity index"
data OAnchor = OAnchor NodeId deriving (Eq,Ord) --Source coming from the output of a device/function
type Link = (OAnchor,IAnchor)

instance Show NodeId where
  show (Id i) = T.unpack i
  show Input  = "input"
  show Output = "output"

instance Show IAnchor where
  show (IAnchor nid indx) = "input_" ++ show nid ++ "_" ++ T.unpack indx

instance Show OAnchor where
  show (OAnchor Input) = "input"
  show (OAnchor nid)   = "output_" ++ show nid

data RouteTable = RouteTable {
                                nodes :: [Node]
                              , links :: [Link]
                             } deriving (Eq,Ord,Show)

--iAnchors :: Node -> [IAnchor]
--iAnchors (Device nid _)    = [IAnchor nid 0]
--iAnchors (PureFun nid _ i) = map (\i -> IAnchor nid i) $ [0..i-1]

oAnchor :: Node -> OAnchor 
oAnchor (Device nid _)    = OAnchor nid 
oAnchor (PureFun nid _ i) = OAnchor nid 

finder :: NodeId -> Node -> Bool
finder id x = case x of
               Device i _    -> i == id
               PureFun i _ _ -> i == id

instance FromJSON LinkNodes where
  parseJSON v = liftM LNS $ withObject "Graph structure" parseGraph v

instance FromJSON LinkNode where
  parseJSON = parseLinkNode -- v <|> parseNode v
  

instance FromJSON NodeId where
  parseJSON (Object v) = do
                           obj_type <- (v .: "node_ident") :: Parser Text
                           case obj_type of
                                "input"  -> return Input
                                "output" -> return Output
                                "node"   -> Id <$> (v .: "id")
  parseJSON (String obj_type) = case obj_type of
                                  "input"  -> return Input
                                  "output" -> return Output
                                  z        -> return $ Id z
  
  parseJSON z = fail $ "NodeID got non-object:\n" ++ show z

instance ToJSON NodeId where
  toJSON Input  = object ["node_ident" .= ("input" :: Text)]
  toJSON Output = object ["node_ident" .= ("output" :: Text)]
  toJSON (Id i) = object ["node_ident" .= ("node" :: Text), "id" .= i]

instance FromJSON Node where
  parseJSON (Object v) = do
                           obj_type <- (v .: "node_type") :: Parser Text
                           case obj_type of
                                "device"   -> Device  <$> (v .: "node_id") <*> (v .: "node_name") 
                                "function" -> PureFun <$> (v .: "node_id") <*> (v .: "node_name") <*> (v .: "node_arity")
  parseJSON _ = mzero

instance ToJSON Node where
  toJSON (Device nid nref) = object ["node_type" .= ("device" :: Text), "node_id" .= nid, "node_name" .= nref]
  toJSON (PureFun nid nref arity) = object ["node_type" .= ("function" :: Text), "node_id" .= nid, "node_name" .= nref, "node_arity" .= arity]

instance FromJSON IAnchor where
  parseJSON (Object v) = IAnchor <$> (v .: "node_id") <*> (v .: "anchor") 
  parseJSON _ = mzero

instance ToJSON IAnchor where
  toJSON (IAnchor nid indx) = object ["node_id" .= nid, "anchor" .= indx]

instance FromJSON OAnchor where
  parseJSON (Object v) = OAnchor <$> (v .: "node_id")  
  parseJSON _ = mzero

instance ToJSON OAnchor where
  toJSON (OAnchor nid) = object ["node_id" .= nid]

instance FromJSON RouteTable where
  parseJSON (Object v) = RouteTable <$> (v .: "nodes") <*> (v .: "links")
  parseJSON _ = mzero
                                              
instance ToJSON RouteTable where
  toJSON (RouteTable nodes links) = object ["nodes" .= nodes, "links" .= links]

testRT = RouteTable {
                      nodes = [
                         (Device (Id "1") "dev1")
                         , (Device (Id "2") "dev2")
                         , (PureFun (Id "3") "mkTuple" 2)
                      ], 
                      links = [
                        (OAnchor Input, IAnchor (Id "1") "0")
                        , (OAnchor Input, IAnchor (Id "2") "0")
                        , (OAnchor (Id "1"), IAnchor (Id "3") "0")
                        , (OAnchor (Id "2"), IAnchor (Id "3") "1")
                        , (OAnchor (Id "3"), IAnchor Output "0")
                      ]
                    }


--instance FromJSON LinkNode where

assert :: Eq a => Parser a -> a -> Parser ()
assert p a = do
                a' <- p
                if a == a'
                  then return ()
                  else empty
                

field_is :: Object -> Text -> Text -> Parser ()
field_is obj k v = assert (obj .: k) v 
                      

parseLinkNode :: Value -> Parser LinkNode  
parseLinkNode (Object obj) = (do field_is obj "type" "link"
                                 source <- obj .: "source"
                                 target <- obj .: "target"
                                 source_id <- source .: "id"
                                 target_id <- target .: "id"
                                 target_port <- target .: "port"
                                 let t = IAnchor (idconv target_id) target_port
                                     s = OAnchor (idconv source_id)
                                 return $ L $ (s,t))
                  <|> (do field_is obj "type" "devs.Model"
                          nodename <- obj .: "nodename"
                          nid      <- obj .: "id"
                          return $ N nid nodename)
  where
    idconv "input"  = Input
    idconv "output" = Output
    idconv txt      = Id txt

parseGraph :: Object -> Parser [LinkNode]
parseGraph obj = do cells <- obj .: "cells"
                    case cells of
                        Array vs ->  do 
                                        nodes <- V.mapM (parseLinkNode) vs
                                        return $ V.toList nodes
                        _        -> empty

test :: B.ByteString
test = "{\"cells\":[{\"type\":\"devs.Model\",\"size\":{\"width\":90,\"height\":90},\"inPorts\":[\"in\"],\"outPorts\":[\"out\"],\"position\":{\"x\":50,\"y\":50},\"angle\":0,\"nodename\":\"hi\",\"id\":\"da739dc5-00ba-440f-9108-2aae41c43933\",\"z\":1,\"attrs\":{\".label\":{\"text\":\"hi\",\"ref-x\":0.4},\"rect\":{\"fill\":\"#2ECC71\",\"rx\":15,\"ry\":15},\".inPorts circle\":{\"fill\":\"#16A085\",\"magnet\":\"passive\",\"type\":\"input\"},\".outPorts circle\":{\"fill\":\"#E74C3C\",\"type\":\"output\"},\".inPorts>.port0>.port-label\":{\"text\":\"in\"},\".inPorts>.port0>.port-body\":{\"port\":{\"id\":\"in\",\"type\":\"in\"}},\".inPorts>.port0\":{\"ref\":\".body\",\"ref-y\":0.5},\".outPorts>.port0>.port-label\":{\"text\":\"out\"},\".outPorts>.port0>.port-body\":{\"port\":{\"id\":\"out\",\"type\":\"out\"}},\".outPorts>.port0\":{\"ref\":\".body\",\"ref-y\":0.5,\"ref-dx\":0}}},{\"type\":\"devs.Model\",\"size\":{\"width\":90,\"height\":90},\"inPorts\":[\"in\"],\"outPorts\":[\"out\"],\"position\":{\"x\":496,\"y\":54},\"angle\":0,\"nodename\":\"hi2\",\"id\":\"ad42f5e8-4e67-4905-ac8f-860499111019\",\"z\":2,\"attrs\":{\".label\":{\"text\":\"hi2\",\"ref-x\":0.4},\"rect\":{\"fill\":\"#2ECC71\",\"rx\":15,\"ry\":15},\".inPorts circle\":{\"fill\":\"#16A085\",\"magnet\":\"passive\",\"type\":\"input\"},\".outPorts circle\":{\"fill\":\"#E74C3C\",\"type\":\"output\"},\".inPorts>.port0>.port-label\":{\"text\":\"in\"},\".inPorts>.port0>.port-body\":{\"port\":{\"id\":\"in\",\"type\":\"in\"}},\".inPorts>.port0\":{\"ref\":\".body\",\"ref-y\":0.5},\".outPorts>.port0>.port-label\":{\"text\":\"out\"},\".outPorts>.port0>.port-body\":{\"port\":{\"id\":\"out\",\"type\":\"out\"}},\".outPorts>.port0\":{\"ref\":\".body\",\"ref-y\":0.5,\"ref-dx\":0}}},{\"type\":\"link\",\"router\":{\"name\":\"metro\"},\"connector\":{\"name\":\"normal\"},\"id\":\"9ad24606-2655-4da7-83ce-0bade2c4d76d\",\"embeds\":\"\",\"source\":{\"id\":\"da739dc5-00ba-440f-9108-2aae41c43933\",\"selector\":\"g:nth-child(1) g:nth-child(4) g:nth-child(1) circle:nth-child(1)     \",\"port\":\"out\"},\"target\":{\"id\":\"ad42f5e8-4e67-4905-ac8f-860499111019\",\"selector\":\"g:nth-child(1) g:nth-child(3) g:nth-child(1) circle:nth-child(1)     \",\"port\":\"in\"},\"z\":3,\"attrs\":{\".connection\":{\"stroke\":\"#333333\",\"stroke-width\":3},\".marker-target\":{\"fill\":\"#333333\",\"d\":\"M 10 0 L 0 5 L 10 10 z\"}}}]}"


