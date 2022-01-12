import Data.Word ( Word8, Word16, Word32 )
import Data.Char ( isDigit )
import Data.List ( foldl1' )

type Address = Word32
type Port    = Word16
type Subnet  = Word8

-- The two following types/aliases are yours to define. You can change the
-- declaration from 'data' (= distinct type) to 'type' (= alias), but you must
-- retain their arities (Packet must remain binary and PacketHeader nullary).

type Source = (Address, Port)
type Destination = (Address, Port)

data Packet header payload = Packet header payload
    deriving (Show)

data PacketHeader = PacketHeader Source Destination
    deriving (Show)


-- Here, 'header' represents a packet header type which does not need to be
-- PacketHeader (and our tests do indeed use their own header type).
type Chain  header = [Rule header]
type Rule   header = ([Match header], Action header)
type Match  header = header -> Bool
data Action header = Accept | Reject | Call (Chain header) | Return

getPayload :: Packet header payload -> payload
getPayload (Packet header payload) = payload

getHeader :: Packet header payload -> header
getHeader (Packet header payload) = header

mkPacket :: header -> payload -> Packet header payload
mkPacket h p = (Packet h p)

mkHeader :: Address -> Port -> Address -> Port -> PacketHeader
mkHeader a1 p1 a2 p2 = (PacketHeader (a1, p1) (a2, p2))



packetFilter :: Chain header -> [Packet header payload] -> [Packet header payload]
packetFilter [] [] = []
packetFilter chain [] = []
packetFilter [] packets = []

packetFilter chain packets = filtering chain packets

                    where

                        filtering _ [] = []
                        filtering (h : t) (p1: rest) = if getValue(traverse (h: t) (getHeader p1))
                                                       then p1 : filtering (h: t) rest
                                                       else filtering (h: t) rest
                                                       

                        isCall (Call _) = True
                        isCall _ = False

                        getCallChain (Call x) = x

                        isAccept (Accept) = True
                        isAccept _ = False

                        isReject (Reject) = True
                        isReject _ = False

                        isReturn (Return) = True
                        isReturn _ = False

                        getValue (Just True) = True
                        getValue (Just False) = False
                        getValue Nothing = False

                        isNothing Nothing = True
                        isNothing _ = False


                        traverse [] header = Nothing
                        traverse (h : t) header = if (check_match (fst h) header [])
                                                         then
                                                            if isCall (snd h) -- if action is call
                                                            then 
                                                                if isNothing (traverse (getCallChain (snd h)) header)
                                                                then traverse t header
                                                                else traverse (getCallChain (snd h)) header
                                                            else
                                                                if isAccept (snd h)
                                                                then (Just True)
                                                                else
                                                                    if isReject (snd h)
                                                                    then (Just False)
                                                                    else
                                                                        if isReturn (snd h)
                                                                        then Nothing
                                                                        else (Just False) -- unreachable

                                                         else traverse t header  -- Does not match


                        check_match [] header res = (and res)
                        check_match (m1: rest) header res = check_match rest header (res ++ [(m1 header)])


-- Helpers
getDst :: PacketHeader -> Destination
getDst (PacketHeader (a1, p1) (a2, p2)) = (a2, p2)


getSrc :: PacketHeader -> Source
getSrc (PacketHeader (a1, p1) (a2, p2)) = (a1, p1)



-- eDSL
match :: ((PacketHeader) -> a) -> (a -> b) -> (c -> b -> Bool) -> c -> Match PacketHeader
match f1 f2 f3 const = (f4.f2.f1)
        where
            f4 = f3 const
 

addr :: Source -> Address
addr (a, p) = a

port :: Source -> Port
port (a, p) = p


src :: (Source -> b) -> (c -> b -> Bool) -> c -> Match PacketHeader
src f1 f2 const = match getSrc f1 f2 const

dst  :: (Destination -> b) -> (c -> b -> Bool) -> c -> Match PacketHeader
dst f1 f2 const = match getDst f1 f2 const

equals :: Eq c => c -> c -> Bool
equals const p = (const == p)

member :: (Eq b) => [b] -> b -> Bool
member const p = (p `elem` const)

within :: (Ord a) => (a, a) -> a -> Bool
within (a, b) p = a <= p && p <= b

subnet :: (Address, Subnet) -> Address -> Bool
subnet net p = p `isInNet` net

-- -------- --
-- EXAMPLES --
-- -------- --

-- NOTE: Many of the examples below are commented out as to not interfere with
-- your work on the eDSL. Uncomment those once you are finished with it.

-- You can test your ‹packetFilter› with simpler packets first:

ex_simplePackets :: [ Packet Int () ]
ex_simplePackets = map (flip mkPacket ()) [-12..12]

ex_simpleChain :: Chain Int
ex_simpleChain = [([ (== 11) ],    Reject)
                 ,([ (< 0), odd ], Reject)
                 ,([ (> 1)],       Call
                             [([ odd ],           Accept)
                             ,([ (== 6) ],        Return)
                             ,([ (> 3), (<= 9) ], Reject)
                             ])
                 ,([ (== 0) . (`mod` 3) ], Accept)
                 ]

-- ‹packetFilter ex_simpleChain ex_simplePackets› should return the following:

ex_simpleExpected :: [ Packet Int () ]
ex_simpleExpected = map (flip mkPacket ()) [-12, -6, 0, 3, 5, 6, 7, 9, 12]

-- These examples are designed to work with the “full” eDSL which does not need
-- explicit conversion of strings to IP addresses. Should you choose to
-- implement only the simpler eDSL, you need to add the ‹ip› calls manually.

-- First, a simple list of all combinations to check that your eDSL is typeable:

ex_rules :: [Match PacketHeader]
ex_rules =
 [ src port equals 23
 , src port within (5000, 5100)
 , src port member [80, 443, 8080]
 , dst port equals 23
 , dst port within (5000, 5100)
 , dst port member [80, 443, 8080]

 --Version with explicit IP address conversion (after step 3 of the tutorial)
 , src addr equals (ip "192.168.0.0")
 , src addr within (ip "192.168.0.0", ip "192.168.255.255")
 , src addr member [ip "192.168.0.1", ip "192.168.0.3"]
 , src addr subnet (ip "192.168.0.0", 16)
 , dst addr equals (ip "192.168.0.0")
 , dst addr within (ip "192.168.0.0", ip "192.168.255.255")
 , dst addr member [ip "192.168.0.1", ip "192.168.0.3"]
 , dst addr subnet (ip "192.168.0.0", 16)

 {- Full version (after step 4 of the tutorial; comment out the previous block
  - of address matching)
 , src addr equals "192.168.0.0"
 , src addr within ("192.168.0.0", "192.168.255.255")
 , src addr member ["192.168.0.1", "192.168.0.3"]
 , src addr subnet ("192.168.0.0", 16)
 , dst addr equals "192.168.0.0"
 , dst addr within ("192.168.0.0", "192.168.255.255")
 , dst addr member ["192.168.0.1", "192.168.0.3"]
 , dst addr subnet ("192.168.0.0", 16)
 -}

 {- Optional bonus: match negation
 , src port ! equals 23
 , dst port ! within (0, 1024)
 , dst addr ! member ["192.168.0.1", "192.168.0.3"]
 , src addr ! subnet ("192.168.0.0", 16)
 -}
 ]


-- Now an example of chains inspired by networking of Faculty of Informatics.
-- We consider anything in the 147.251.48.0/20 network (that is, addresses from
-- 147.251.48.0 to 147.251.63.255) to be in “the FI network”.
--
-- There is one main chain and three supplementary named ones to make the rules
-- a bit more readable.

-- Uncomment to enable the bigger example.

ex_chainSsh, ex_chainOnlyFI, ex_chainAllowOutbound :: Chain PacketHeader

-- SSH uses port 22. Calling this chains is just a slightly more readable
-- way to allow incoming SSH traffic.
ex_chainSsh = [([dst port equals 22], Accept)]

-- Calling this chain rejects anything not from the FI network.
-- Note that we return (not accept) on a FI-originating packet, so that next
-- rules are used.
ex_chainOnlyFI =
    [([src addr subnet (ip "147.251.48.0", 20)], Return)
    ,([], Reject)
    ]
-- Calling this chain accepts anything that goes outside FI. Packets coming into
-- tho FI network must be decided by subsequent rules.
ex_chainAllowOutbound =
    [([dst addr subnet (ip "147.251.48.0", 20)], Return)
    ,([], Accept)
    ]

ex_chainMain :: Chain PacketHeader
ex_chainMain =
    -- Allow all comunication from FI to the outside world
    [([src addr subnet (ip "147.251.48.0", 20)], Call ex_chainAllowOutbound)

    -- Accept traffic to dynamic ports inside FI
    ,([dst addr subnet (ip "147.251.48.0", 20), dst port within (49152, 65535)], Accept)

    -- Aisa (is an SSH server and a webserver)
    ,([dst addr equals (ip "147.251.48.1")], Call
            [ ([], Call ex_chainSsh)
            , ([dst port member [80, 443]], Accept) -- 80 = HTTP, 443 = HTTPS
            ] )
    -- Nymfe{01..105} (SSH, only from within FI networks)
    ,([dst addr within (ip "147.251.53.11", ip "147.251.53.115")], Call
            [([], Call ex_chainOnlyFI)
            ,([], Call ex_chainSsh)
            ] )
    -- Drop all other trafic (default rule when the main chain falls through)
    ]

ex_packets :: [Packet PacketHeader String]
ex_packets = [ mkPacket (mkHeader (ip sip) sport (ip dip) dport) desc
             | (sip, sdesc) <- hosts
             , (dip, ddesc) <- hosts
             , (sport, spdesc) <- sports
             , (dport, dpdesc) <- dports
             , sip /= dip
             , let desc = unwords [ sdesc, spdesc, "-->", ddesc, dpdesc ]
             ]
    where hosts = [("147.251.48.1",   "aisa")
                  ,("147.251.53.11",  "nymfe01")
                  ,("55.55.75.67",    "outsider")
                  ]
          sports = [(55550, "dynamic") ]
          dports = [(   22, "ssh")
                   ,(  443, "https")
                   ] ++ sports


ex_filtered :: [String]
ex_filtered = map getPayload $ packetFilter ex_chainMain ex_packets

-- The following packets (and only these) must be in ‹ex_filtered›:
ex_filteredExpected :: [String]
ex_filteredExpected = [ "aisa dynamic --> nymfe01 ssh"
                      , "aisa dynamic --> nymfe01 dynamic"
                      , "aisa dynamic --> outsider ssh"
                      , "aisa dynamic --> outsider https"
                      , "aisa dynamic --> outsider dynamic"
                      , "nymfe01 dynamic --> aisa ssh"
                      , "nymfe01 dynamic --> aisa https"
                      , "nymfe01 dynamic --> aisa dynamic"
                      , "nymfe01 dynamic --> outsider ssh"
                      , "nymfe01 dynamic --> outsider https"
                      , "nymfe01 dynamic --> outsider dynamic"
                      , "outsider dynamic --> aisa ssh"
                      , "outsider dynamic --> aisa https"
                      , "outsider dynamic --> aisa dynamic"
                      , "outsider dynamic --> nymfe01 dynamic"
                      ]


-- ------------------------- --
-- PROVIDED HELPER FUNCTIONS --
-- ------------------------- --

-- Evaluates to True when the address in the first argument belongs to the
-- subnet specified by the second argument.
-- 'prefixLength' is the number of most significant bits of 'net' which describe
-- the net (i.e., the number after the slash in CIDR notation).
isInNet :: Address -> (Address, Subnet) -> Bool
isInNet _ (  _,            0) = True
isInNet a (net, prefixLength) = if prefixLength <= 32
                                then a >= low && a <= high
                                else error "Prefix longer than entire address"
    where hostMask = 2 ^ (32 - prefixLength)
          low      = net - (net `mod` hostMask)
          high     = low + (hostMask - 1)

-- Converts an IPv4 address from the usual dot notation to a 32-bit number.
--
-- Note that this is not what a pretty converting function in Haskell looks
-- like, but we wanted to keep it independent on external libraries nor we
-- wished to use advanced techniques that you are not familiar with.
ip :: String -> Address
ip addr = let
        inOctets = splitOctets addr
        nOctets  = length inOctets
        octets   = take 4 $ inOctets ++ repeat 0
    in
        if nOctets > 4
        then error "Too many octets"
        else foldl1' (\a o -> a * 256 + o) octets
    where
        splitOctets :: String -> [Word32]
        splitOctets addr = let (o, rest) = span isDigit addr
                               octet = read o in
                      if null o      then error "Empty octet" else
                      if octet > 255 then error "Number too big for octet"
                                     else octet : checkAndSplit rest
        checkAndSplit [] = []
        checkAndSplit ('.' : rest) = splitOctets rest
        checkAndSplit _ = error "Expected dot or end of string"

