{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{- | The `flat` files containing validation scripts in `plutus-benchmarks` were
generated in August 2021.  In May 2022 it was observed that deserialising them
to memory and then reserialising them usually caused the size of the `flat`
files to increase.  This was because the CBOR encoding of `Data` objects had
changed: it formerly used a definite-length encoding for lists but changed to
using an indefinite-length encoding, which requires an extra 0xff tag to mark
the end of the list (see Section 3.2 of RFC 8949). The serialised versions show
significant differences (see `testData` below) but deserialise to identical
objects.  These tests contain pairs of different encodings of Data objects from
a selection of the scripts and check that they continue to deserialise to the
same thing, and that both deserialise to the expected object. -}

module CBOR.DataStability (tests)
where

import PlutusCore.Data

import Codec.Serialise (deserialise)
import Data.ByteString.Lazy qualified as BSL (fromStrict, length)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Text.Hex (decodeHex)
import Text.Printf (printf)

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "CBOR"
        [
         testGroup "data-cbor-stability" (fmap maketest testData)
        ]

-- | Convert the Text objects in the testData entries to bytestrings and then
-- deserialise them and check that both give the same Data object and that both
-- match the expected object.  Without the bangs, error messages can appear in
-- misleading places.
maketest :: (String, Text, Text, Data) -> TestTree
maketest (name, s1, s2, expected) =
    testCaseSteps name $ \step -> do
      let !bs1 = BSL.fromStrict . fromJust . decodeHex $ s1
          size1 = BSL.length bs1
      step $ printf "Deserialising Data object 1 from CBOR bytestring (%s)" (numBytes size1)
      let !object1 = deserialise @Data bs1
      let !bs2 = BSL.fromStrict . fromJust . decodeHex $ s2
          size2 = BSL.length bs2
      step $ printf "Deserialising Data object 2 from CBOR bytestring (%s)" (numBytes size2)
      let !object2 = deserialise @Data bs2
      step $ printf "CBOR size difference: %s" (numBytes (size2 - size1))
      step "Checking that object1 decoded correctly"
      object1 @?= expected
      step "Checking that object2 decoded correctly"
      object2 @?= expected
      step "Checking object1 and object2 for equality"
      assertEqual "Deserialisation produced different objects" object1 object2
          where numBytes s =
                    if s == 1
                    then "1 byte"
                    else printf "%d bytes" s :: String

-- A list of tuples containing (Script/data-item name, encoded version 1,
-- encoded version 2, expected object)
testData :: [(String, Text, Text, Data)]
testData =
    [
     ( "auction_1-1"
     , "d87982581cbd99a373075d42fe4ac9109515e46303d0940cb9620bf058b87986a9d87980"
     , "d8799f581cbd99a373075d42fe4ac9109515e46303d0940cb9620bf058b87986a9d87980ff"
     , Constr 0 [B "\189\153\163s\a]B\254J\201\DLE\149\NAK\228c\ETX\208\148\f\185b\v\240X\184y\134\169", Constr 0 []]
     )
    ,
     ( "future-increase-margin-5 (a)"
     , "d87981d8798281d879824081d879824019084b81d879824081d87982401908af"
     , "d8799fd8799f9fd8799f409fd8799f4019084bffffffff9fd8799f409fd8799f401908afffffffffffff"
     , Constr 0 [Constr 0 [List [Constr 0 [B "",List [Constr 0 [B "",I 2123]]]],List [Constr 0 [B "",List [Constr 0 [B "",I 2223]]]]]]
     )
    ,
     ( "future-increase-margin-5 (b)"
     , "d87a81d879835840c40f1dc048cc8b8b490cf6f58fbb582f01ecb8199094ed84961ab079dda95df47930e1607f41806587229912a670b64f2c6e67db22c2187781fce00df43c240f5820d8af98eecf2d0c875462713ae861164de002f9a0830f01b19dc33a4e27592513d8798281d879824081d87982401904641b000001739c8a86d8"
     , "d87a9fd8799f5840c40f1dc048cc8b8b490cf6f58fbb582f01ecb8199094ed84961ab079dda95df47930e1607f41806587229912a670b64f2c6e67db22c2187781fce00df43c240f5820d8af98eecf2d0c875462713ae861164de002f9a0830f01b19dc33a4e27592513d8799f9fd8799f409fd8799f40190464ffffffff1b000001739c8a86d8ffffff"
     , Constr 1 [Constr 0 [B "\196\SI\GS\192H\204\139\139I\f\246\245\143\187X/\SOH\236\184\EM\144\148\237\132\150\SUB\176y\221\169]\244y0\225`\DELA\128e\135\"\153\DC2\166p\182O,ng\219\"\194\CANw\129\252\224\r\244<$\SI",B "\216\175\152\238\207-\f\135Tbq:\232a\SYNM\224\STX\249\160\131\SI\SOH\177\157\195:N'Y%\DC3",Constr 0 [List [Constr 0 [B "",List [Constr 0 [B "",I 1124]]]],I 1596059191000]]]
     )
    ,
     ( "future-increase-margin-5 (c)"
     , "d87982d8798a82d87982d87982d87981582044ab203a84db9ef946495a1cf1290d27ed80a4538b389fe6e3b39ef1d44f615700d87983d87982d87981581c977efb35ab621d39dbeb7274ec7795a34708ff4d25a01a1df04c1f27d87a8081d879824081d87982401a05f5aaa2d87a80d87982d87982d87981582044ab203a84db9ef946495a1cf1290d27ed80a4538b389fe6e3b39ef1d44f615701d87983d87982d87a81581c12b132132c2b41a484de3b7a5db19be8ca28441de2b4148609079d19d87a8081d879824081d87982401910fad8798158208eb8c339886d3979c8edaf8feafd5fd410e127f312d3c6fcec8b5f979e68957283d87983d87982d87981581c977efb35ab621d39dbeb7274ec7795a34708ff4d25a01a1df04c1f27d87a8081d879824081d87982401a05f57cfdd87a80d87983d87982d87a81581c08535dc84e7823d63a787f1229ff1863e6c009907093b384289d50cdd87a8081d879824081d8798240190790d8798158202cdb268baecefad822e5712f9e690e1787f186f5c84c343ffdc060b21f0241e0d87983d87982d87a81581cea16da21adb923d789313ccef2644d9ea19c2141f8f92bada80efb06d87a8081d879824081d879824019096ad8798158202cdb268baecefad822e5712f9e690e1787f186f5c84c343ffdc060b21f0241e081d879824081d8798240192da5808080d87982d87982d87a811b000001739c8a86d8d87a80d87982d87b80d87a8081581c977efb35ab621d39dbeb7274ec7795a34708ff4d25a01a1df04c1f2782d8798258202cdb268baecefad822e5712f9e690e1787f186f5c84c343ffdc060b21f0241e0d87980d879825820d8af98eecf2d0c875462713ae861164de002f9a0830f01b19dc33a4e27592513d8798281d879824081d87982401904641b000001739c8a86d8d879815820db145e448a2af884a7430ac31cec85bc700a89f35f1feb71544b5e31f4c68446d87a81d87982d87981582044ab203a84db9ef946495a1cf1290d27ed80a4538b389fe6e3b39ef1d44f615701"
     , "d8799fd8799f9fd8799fd8799fd8799f582044ab203a84db9ef946495a1cf1290d27ed80a4538b389fe6e3b39ef1d44f6157ff00ffd8799fd8799fd8799f581c977efb35ab621d39dbeb7274ec7795a34708ff4d25a01a1df04c1f27ffd87a80ff9fd8799f409fd8799f401a05f5aaa2ffffffffd87a80ffffd8799fd8799fd8799f582044ab203a84db9ef946495a1cf1290d27ed80a4538b389fe6e3b39ef1d44f6157ff01ffd8799fd8799fd87a9f581c12b132132c2b41a484de3b7a5db19be8ca28441de2b4148609079d19ffd87a80ff9fd8799f409fd8799f401910faffffffffd8799f58208eb8c339886d3979c8edaf8feafd5fd410e127f312d3c6fcec8b5f979e689572ffffffff9fd8799fd8799fd8799f581c977efb35ab621d39dbeb7274ec7795a34708ff4d25a01a1df04c1f27ffd87a80ff9fd8799f409fd8799f401a05f57cfdffffffffd87a80ffd8799fd8799fd87a9f581c08535dc84e7823d63a787f1229ff1863e6c009907093b384289d50cdffd87a80ff9fd8799f409fd8799f40190790ffffffffd8799f58202cdb268baecefad822e5712f9e690e1787f186f5c84c343ffdc060b21f0241e0ffffd8799fd8799fd87a9f581cea16da21adb923d789313ccef2644d9ea19c2141f8f92bada80efb06ffd87a80ff9fd8799f409fd8799f4019096affffffffd8799f58202cdb268baecefad822e5712f9e690e1787f186f5c84c343ffdc060b21f0241e0ffffff9fd8799f409fd8799f40192da5ffffffff808080d8799fd8799fd87a9f1b000001739c8a86d8ffd87a80ffd8799fd87b80d87a80ffff9f581c977efb35ab621d39dbeb7274ec7795a34708ff4d25a01a1df04c1f27ff9fd8799f58202cdb268baecefad822e5712f9e690e1787f186f5c84c343ffdc060b21f0241e0d87980ffd8799f5820d8af98eecf2d0c875462713ae861164de002f9a0830f01b19dc33a4e27592513d8799f9fd8799f409fd8799f40190464ffffffff1b000001739c8a86d8ffffffd8799f5820db145e448a2af884a7430ac31cec85bc700a89f35f1feb71544b5e31f4c68446ffffd87a9fd8799fd8799f582044ab203a84db9ef946495a1cf1290d27ed80a4538b389fe6e3b39ef1d44f6157ff01ffffff"
     , Constr 0 [Constr 0 [List [Constr 0 [Constr 0 [Constr 0 [B "D\171 :\132\219\158\249FIZ\FS\241)\r'\237\128\164S\139\&8\159\230\227\179\158\241\212OaW"],I 0],Constr 0 [Constr 0 [Constr 0 [B "\151~\251\&5\171b\GS9\219\235rt\236w\149\163G\b\255M%\160\SUB\GS\240L\US'"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 99986082]]]],Constr 1 []]],Constr 0 [Constr 0 [Constr 0 [B "D\171 :\132\219\158\249FIZ\FS\241)\r'\237\128\164S\139\&8\159\230\227\179\158\241\212OaW"],I 1],Constr 0 [Constr 0 [Constr 1 [B "\DC2\177\&2\DC3,+A\164\132\222;z]\177\155\232\202(D\GS\226\180\DC4\134\t\a\157\EM"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 4346]]]],Constr 0 [B "\142\184\195\&9\136m9y\200\237\175\143\234\253_\212\DLE\225'\243\DC2\211\198\252\236\139_\151\158h\149r"]]]],List [Constr 0 [Constr 0 [Constr 0 [B "\151~\251\&5\171b\GS9\219\235rt\236w\149\163G\b\255M%\160\SUB\GS\240L\US'"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 99974397]]]],Constr 1 []],Constr 0 [Constr 0 [Constr 1 [B "\bS]\200Nx#\214:x\DEL\DC2)\255\CANc\230\192\t\144p\147\179\132(\157P\205"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 1936]]]],Constr 0 [B ",\219&\139\174\206\250\216\"\229q/\158i\SO\ETB\135\241\134\245\200L4?\253\192`\178\US\STXA\224"]],Constr 0 [Constr 0 [Constr 1 [B "\234\SYN\218!\173\185#\215\137\&1<\206\242dM\158\161\156!A\248\249+\173\168\SO\251\ACK"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 2410]]]],Constr 0 [B ",\219&\139\174\206\250\216\"\229q/\158i\SO\ETB\135\241\134\245\200L4?\253\192`\178\US\STXA\224"]]],List [Constr 0 [B "",List [Constr 0 [B "",I 11685]]]],List [],List [],List [],Constr 0 [Constr 0 [Constr 1 [I 1596059191000],Constr 1 []],Constr 0 [Constr 2 [],Constr 1 []]],List [B "\151~\251\&5\171b\GS9\219\235rt\236w\149\163G\b\255M%\160\SUB\GS\240L\US'"],List [Constr 0 [B ",\219&\139\174\206\250\216\"\229q/\158i\SO\ETB\135\241\134\245\200L4?\253\192`\178\US\STXA\224",Constr 0 []],Constr 0 [B "\216\175\152\238\207-\f\135Tbq:\232a\SYNM\224\STX\249\160\131\SI\SOH\177\157\195:N'Y%\DC3",Constr 0 [List [Constr 0 [B "",List [Constr 0 [B "",I 1124]]]],I 1596059191000]]],Constr 0 [B "\219\DC4^D\138*\248\132\167C\n\195\FS\236\133\188p\n\137\243_\US\235qTK^1\244\198\132F"]],Constr 1 [Constr 0 [Constr 0 [B "D\171 :\132\219\158\249FIZ\FS\241)\r'\237\128\164S\139\&8\159\230\227\179\158\241\212OaW"],I 1]]]
     )
    ,
     ( "uniswap-1"
     , "d87982d8798a81d87982d87982d8798158200636250aef275497b4f3807d661a299e34e53e5ad3bc1110e43d1f3420bc8fae06d87983d87982d87981581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3d87a8081d879824081d87982401a05f5e100d87a8081d87983d87982d87981581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3d87a8082d87982581c60bab65c3fbe2ba8c920d0ba15cdb2489767c81349271ecd5bbbeb7684d8798241411a003d0900d8798241421a003d0900d8798241431a003d0900d8798241441a003d0900d879824081d87982401a05f5cf58d87a8081d879824081d87982401911a881d87982581c60bab65c3fbe2ba8c920d0ba15cdb2489767c81349271ecd5bbbeb7684d8798241411a003d0900d8798241421a003d0900d8798241431a003d0900d8798241441a003d09008080d87982d87982d87980d87a80d87982d87b80d87a8081581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a380d879815820f154625e0831084f4981dd205ec3d7dc93d87951645fd963d445d5c2075d982bd87981581c60bab65c3fbe2ba8c920d0ba15cdb2489767c81349271ecd5bbbeb76"
     , "d8799fd8799f9fd8799fd8799fd8799f58200636250aef275497b4f3807d661a299e34e53e5ad3bc1110e43d1f3420bc8faeff06ffd8799fd8799fd8799f581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3ffd87a80ff9fd8799f409fd8799f401a05f5e100ffffffffd87a80ffffff9fd8799fd8799fd8799f581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3ffd87a80ff9fd8799f581c60bab65c3fbe2ba8c920d0ba15cdb2489767c81349271ecd5bbbeb769fd8799f41411a003d0900ffd8799f41421a003d0900ffd8799f41431a003d0900ffd8799f41441a003d0900ffffffd8799f409fd8799f401a05f5cf58ffffffffd87a80ffff9fd8799f409fd8799f401911a8ffffffff9fd8799f581c60bab65c3fbe2ba8c920d0ba15cdb2489767c81349271ecd5bbbeb769fd8799f41411a003d0900ffd8799f41421a003d0900ffd8799f41431a003d0900ffd8799f41441a003d0900ffffffff8080d8799fd8799fd87980d87a80ffd8799fd87b80d87a80ffff9f581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3ff80d8799f5820f154625e0831084f4981dd205ec3d7dc93d87951645fd963d445d5c2075d982bffffd8799f581c60bab65c3fbe2ba8c920d0ba15cdb2489767c81349271ecd5bbbeb76ffff"
     , Constr 0 [Constr 0 [List [Constr 0 [Constr 0 [Constr 0 [B "\ACK6%\n\239'T\151\180\243\128}f\SUB)\158\&4\229>Z\211\188\DC1\DLE\228=\US4 \188\143\174"],I 6],Constr 0 [Constr 0 [Constr 0 [B "5\222\221)\130\160<\243\158}\206\ETX\200\&9\153O\253\236.\198\176O\FS\242\212\SOa\163"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 100000000]]]],Constr 1 []]]],List [Constr 0 [Constr 0 [Constr 0 [B "5\222\221)\130\160<\243\158}\206\ETX\200\&9\153O\253\236.\198\176O\FS\242\212\SOa\163"],Constr 1 []],List [Constr 0 [B "`\186\182\\?\190+\168\201 \208\186\NAK\205\178H\151g\200\DC3I'\RS\205[\187\235v",List [Constr 0 [B "A",I 4000000],Constr 0 [B "B",I 4000000],Constr 0 [B "C",I 4000000],Constr 0 [B "D",I 4000000]]],Constr 0 [B "",List [Constr 0 [B "",I 99995480]]]],Constr 1 []]],List [Constr 0 [B "",List [Constr 0 [B "",I 4520]]]],List [Constr 0 [B "`\186\182\\?\190+\168\201 \208\186\NAK\205\178H\151g\200\DC3I'\RS\205[\187\235v",List [Constr 0 [B "A",I 4000000],Constr 0 [B "B",I 4000000],Constr 0 [B "C",I 4000000],Constr 0 [B "D",I 4000000]]]],List [],List [],Constr 0 [Constr 0 [Constr 0 [],Constr 1 []],Constr 0 [Constr 2 [],Constr 1 []]],List [B "5\222\221)\130\160<\243\158}\206\ETX\200\&9\153O\253\236.\198\176O\FS\242\212\SOa\163"],List [],Constr 0 [B "\241Tb^\b1\bOI\129\221 ^\195\215\220\147\216yQd_\217c\212E\213\194\a]\152+"]],Constr 0 [B "`\186\182\\?\190+\168\201 \208\186\NAK\205\178H\151g\200\DC3I'\RS\205[\187\235v"]]
     )
    ,
     ( "escrow-refund-1"
     , "d87982d8798a82d87982d87982d8798158201a630ffeb3be9a107de0a948ce58c23ca5698e000925c5dbb8e69e1966657a3900d87983d87982d87981581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3d87a8081d879824081d87982401a05f5e0e2d87a80d87982d87982d8798158201a630ffeb3be9a107de0a948ce58c23ca5698e000925c5dbb8e69e1966657a3901d87983d87982d87a81581ce7cf3ddee4924dbef3fdceb67f67f11057fe57636443bc3b2ae498f3d87a8081d879824081d879824014d879815820ca54c8836c475a77c6914b4fd598080acadb0f0067778773484d2c12ae7dc75681d87983d87982d87981581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3d87a8081d879824081d87982401a05f5c936d87a8081d879824081d87982401917c0808080d87982d87982d87a811b000001739c8a8abfd87980d87982d87b80d87a8081581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a380d8798158202affd3baa0837dd427ae8924f5fb9729577a25280c4c24dd055f8184fd5b5513d87a81d87982d8798158201a630ffeb3be9a107de0a948ce58c23ca5698e000925c5dbb8e69e1966657a3901"
     , "d8799fd8799f9fd8799fd8799fd8799f58201a630ffeb3be9a107de0a948ce58c23ca5698e000925c5dbb8e69e1966657a39ff00ffd8799fd8799fd8799f581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3ffd87a80ff9fd8799f409fd8799f401a05f5e0e2ffffffffd87a80ffffd8799fd8799fd8799f58201a630ffeb3be9a107de0a948ce58c23ca5698e000925c5dbb8e69e1966657a39ff01ffd8799fd8799fd87a9f581ce7cf3ddee4924dbef3fdceb67f67f11057fe57636443bc3b2ae498f3ffd87a80ff9fd8799f409fd8799f4014ffffffffd8799f5820ca54c8836c475a77c6914b4fd598080acadb0f0067778773484d2c12ae7dc756ffffffff9fd8799fd8799fd8799f581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3ffd87a80ff9fd8799f409fd8799f401a05f5c936ffffffffd87a80ffff9fd8799f409fd8799f401917c0ffffffff808080d8799fd8799fd87a9f1b000001739c8a8abfffd87980ffd8799fd87b80d87a80ffff9f581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3ff80d8799f58202affd3baa0837dd427ae8924f5fb9729577a25280c4c24dd055f8184fd5b5513ffffd87a9fd8799fd8799f58201a630ffeb3be9a107de0a948ce58c23ca5698e000925c5dbb8e69e1966657a39ff01ffffff"
     , Constr 0 [Constr 0 [List [Constr 0 [Constr 0 [Constr 0 [B "\SUBc\SI\254\179\190\154\DLE}\224\169H\206X\194<\165i\142\NUL\t%\197\219\184\230\158\EMfez9"],I 0],Constr 0 [Constr 0 [Constr 0 [B "5\222\221)\130\160<\243\158}\206\ETX\200\&9\153O\253\236.\198\176O\FS\242\212\SOa\163"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 99999970]]]],Constr 1 []]],Constr 0 [Constr 0 [Constr 0 [B "\SUBc\SI\254\179\190\154\DLE}\224\169H\206X\194<\165i\142\NUL\t%\197\219\184\230\158\EMfez9"],I 1],Constr 0 [Constr 0 [Constr 1 [B "\231\207=\222\228\146M\190\243\253\206\182\DELg\241\DLEW\254WcdC\188;*\228\152\243"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 20]]]],Constr 0 [B "\202T\200\131lGZw\198\145KO\213\152\b\n\202\219\SI\NULgw\135sHM,\DC2\174}\199V"]]]],List [Constr 0 [Constr 0 [Constr 0 [B "5\222\221)\130\160<\243\158}\206\ETX\200\&9\153O\253\236.\198\176O\FS\242\212\SOa\163"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 99993910]]]],Constr 1 []]],List [Constr 0 [B "",List [Constr 0 [B "",I 6080]]]],List [],List [],List [],Constr 0 [Constr 0 [Constr 1 [I 1596059191999],Constr 0 []],Constr 0 [Constr 2 [],Constr 1 []]],List [B "5\222\221)\130\160<\243\158}\206\ETX\200\&9\153O\253\236.\198\176O\FS\242\212\SOa\163"],List [],Constr 0 [B "*\255\211\186\160\131}\212'\174\137$\245\251\151)Wz%(\fL$\221\ENQ_\129\132\253[U\DC3"]],Constr 1 [Constr 0 [Constr 0 [B "\SUBc\SI\254\179\190\154\DLE}\224\169H\206X\194<\165i\142\NUL\t%\197\219\184\230\158\EMfez9"],I 1]]]
     )
    ,
     ( "multisig-sm-2"
     , "d87982d8798a82d87982d87982d87981582032ff8f542683ea69e6e1fa23df0b62847f1c0dd9dff8d65230ed1828157aa42a01d87983d87982d87a81581c38034c703e2192479f193f97fe0437fc00d26c0758d4faa749f4c2b8d87a8081d879824081d87982400ad87981582021588ed1ce48a9a88b74e143fb697199fa8da46edf04bb2d3da8970266f51849d87982d87982d879815820ca554bdd535583761a73982bbaa329ace796e73290cf41f10b5208f09138aacb00d87983d87982d87981581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3d87a8081d879824081d87982401a05f5e0ecd87a8082d87983d87982d87981581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3d87a8081d879824081d87982401a05f5b8cad87a80d87983d87982d87a81581c38034c703e2192479f193f97fe0437fc00d26c0758d4faa749f4c2b8d87a8081d879824081d87982400ad879815820cc76070aa0f027db5b3eeeb58810fa57e2d45743be93bf65cb63a288d5f4496581d879824081d8798240192822808080d87982d87982d87980d87a80d87982d87b80d87a8081581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a381d879825820cc76070aa0f027db5b3eeeb58810fa57e2d45743be93bf65cb63a288d5f44965d87a82d8798381d879824081d879824005581c977efb35ab621d39dbeb7274ec7795a34708ff4d25a01a1df04c1f271b000001739c894e5881581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3d87981582022fe5664b828b42c51aa9accb87a448808a89978e8adcd2788b04b204f704664d87a81d87982d87981582032ff8f542683ea69e6e1fa23df0b62847f1c0dd9dff8d65230ed1828157aa42a01"
     , "d8799fd8799f9fd8799fd8799fd8799f582032ff8f542683ea69e6e1fa23df0b62847f1c0dd9dff8d65230ed1828157aa42aff01ffd8799fd8799fd87a9f581c38034c703e2192479f193f97fe0437fc00d26c0758d4faa749f4c2b8ffd87a80ff9fd8799f409fd8799f400affffffffd8799f582021588ed1ce48a9a88b74e143fb697199fa8da46edf04bb2d3da8970266f51849ffffffd8799fd8799fd8799f5820ca554bdd535583761a73982bbaa329ace796e73290cf41f10b5208f09138aacbff00ffd8799fd8799fd8799f581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3ffd87a80ff9fd8799f409fd8799f401a05f5e0ecffffffffd87a80ffffff9fd8799fd8799fd8799f581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3ffd87a80ff9fd8799f409fd8799f401a05f5b8caffffffffd87a80ffd8799fd8799fd87a9f581c38034c703e2192479f193f97fe0437fc00d26c0758d4faa749f4c2b8ffd87a80ff9fd8799f409fd8799f400affffffffd8799f5820cc76070aa0f027db5b3eeeb58810fa57e2d45743be93bf65cb63a288d5f44965ffffff9fd8799f409fd8799f40192822ffffffff808080d8799fd8799fd87980d87a80ffd8799fd87b80d87a80ffff9f581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3ff9fd8799f5820cc76070aa0f027db5b3eeeb58810fa57e2d45743be93bf65cb63a288d5f44965d87a9fd8799f9fd8799f409fd8799f4005ffffffff581c977efb35ab621d39dbeb7274ec7795a34708ff4d25a01a1df04c1f271b000001739c894e58ff9f581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3ffffffffd8799f582022fe5664b828b42c51aa9accb87a448808a89978e8adcd2788b04b204f704664ffffd87a9fd8799fd8799f582032ff8f542683ea69e6e1fa23df0b62847f1c0dd9dff8d65230ed1828157aa42aff01ffffff"
     , Constr 0 [Constr 0 [List [Constr 0 [Constr 0 [Constr 0 [B "2\255\143T&\131\234i\230\225\250#\223\vb\132\DEL\FS\r\217\223\248\214R0\237\CAN(\NAKz\164*"],I 1],Constr 0 [Constr 0 [Constr 1 [B "8\ETXLp>!\146G\159\EM?\151\254\EOT7\252\NUL\210l\aX\212\250\167I\244\194\184"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 10]]]],Constr 0 [B "!X\142\209\206H\169\168\139t\225C\251iq\153\250\141\164n\223\EOT\187-=\168\151\STXf\245\CANI"]]],Constr 0 [Constr 0 [Constr 0 [B "\202UK\221SU\131v\SUBs\152+\186\163)\172\231\150\231\&2\144\207A\241\vR\b\240\145\&8\170\203"],I 0],Constr 0 [Constr 0 [Constr 0 [B "5\222\221)\130\160<\243\158}\206\ETX\200\&9\153O\253\236.\198\176O\FS\242\212\SOa\163"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 99999980]]]],Constr 1 []]]],List [Constr 0 [Constr 0 [Constr 0 [B "5\222\221)\130\160<\243\158}\206\ETX\200\&9\153O\253\236.\198\176O\FS\242\212\SOa\163"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 99989706]]]],Constr 1 []],Constr 0 [Constr 0 [Constr 1 [B "8\ETXLp>!\146G\159\EM?\151\254\EOT7\252\NUL\210l\aX\212\250\167I\244\194\184"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 10]]]],Constr 0 [B "\204v\a\n\160\240'\219[>\238\181\136\DLE\250W\226\212WC\190\147\191e\203c\162\136\213\244Ie"]]],List [Constr 0 [B "",List [Constr 0 [B "",I 10274]]]],List [],List [],List [],Constr 0 [Constr 0 [Constr 0 [],Constr 1 []],Constr 0 [Constr 2 [],Constr 1 []]],List [B "5\222\221)\130\160<\243\158}\206\ETX\200\&9\153O\253\236.\198\176O\FS\242\212\SOa\163"],List [Constr 0 [B "\204v\a\n\160\240'\219[>\238\181\136\DLE\250W\226\212WC\190\147\191e\203c\162\136\213\244Ie",Constr 1 [Constr 0 [List [Constr 0 [B "",List [Constr 0 [B "",I 5]]]],B "\151~\251\&5\171b\GS9\219\235rt\236w\149\163G\b\255M%\160\SUB\GS\240L\US'",I 1596059111000],List [B "5\222\221)\130\160<\243\158}\206\ETX\200\&9\153O\253\236.\198\176O\FS\242\212\SOa\163"]]]],Constr 0 [B "\"\254Vd\184(\180,Q\170\154\204\184zD\136\b\168\153x\232\173\205'\136\176K OpFd"]],Constr 1 [Constr 0 [Constr 0 [B "2\255\143T&\131\234i\230\225\250#\223\vb\132\DEL\FS\r\217\223\248\214R0\237\CAN(\NAKz\164*"],I 1]]]
     )
    ,
     ( "crowdfunding-success-2"
     , "d87982d8798a84d87982d87982d8798158200636250aef275497b4f3807d661a299e34e53e5ad3bc1110e43d1f3420bc8fae06d87983d87982d87981581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3d87a8081d879824081d87982401a05f5e100d87a80d87982d87982d87981582009c21de7ece5b224ead247754e2fb80ce2dd69eb180d29286612a7c55ec05d3c01d87983d87982d87a81581c5e40a47ab6e241233bcd9eaede9220743c5e829c105dbc65b3ffa809d87a8081d879824081d87982401864d87981582002aa535e8c850b40786b9a6c169072d3368b8fb67833413db7bf893bdd4a46f1d87982d87982d8798158206ee5de7047be901322af0e1ff107ce911237f0b60ea38cd935360cbeca8b1cb301d87983d87982d87a81581c5e40a47ab6e241233bcd9eaede9220743c5e829c105dbc65b3ffa809d87a8081d879824081d87982401819d8798158203999f2739f0bbcb9727893a0d2e8cae660f0ce36a73e42a58a7102894473a7e0d87982d87982d879815820f9d5959ed383550d28c45b30dce80260df7bf4741392f2ec1e3e743aa071c55601d87983d87982d87a81581c5e40a47ab6e241233bcd9eaede9220743c5e829c105dbc65b3ffa809d87a8081d879824081d87982401864d879815820509b58c2b6fe87f4888de7c11e6ba23ba34a19cadec76b1b7e7904f39ba0608a81d87983d87982d87981581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3d87a8081d879824081d87982401a05f5ab11d87a8081d879824081d87982401936d0808080d87982d87982d87a811b000001739c894e58d87a80d87982d87a811b000001739c897567d87a8081581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a380d879815820a50a890e9f6b1e4ca495b72cc681e5d52061ef26d49bdd075f2fa8c182af1140d87a81d87982d8798158206ee5de7047be901322af0e1ff107ce911237f0b60ea38cd935360cbeca8b1cb301"
     , "d8799fd8799f9fd8799fd8799fd8799f58200636250aef275497b4f3807d661a299e34e53e5ad3bc1110e43d1f3420bc8faeff06ffd8799fd8799fd8799f581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3ffd87a80ff9fd8799f409fd8799f401a05f5e100ffffffffd87a80ffffd8799fd8799fd8799f582009c21de7ece5b224ead247754e2fb80ce2dd69eb180d29286612a7c55ec05d3cff01ffd8799fd8799fd87a9f581c5e40a47ab6e241233bcd9eaede9220743c5e829c105dbc65b3ffa809ffd87a80ff9fd8799f409fd8799f401864ffffffffd8799f582002aa535e8c850b40786b9a6c169072d3368b8fb67833413db7bf893bdd4a46f1ffffffd8799fd8799fd8799f58206ee5de7047be901322af0e1ff107ce911237f0b60ea38cd935360cbeca8b1cb3ff01ffd8799fd8799fd87a9f581c5e40a47ab6e241233bcd9eaede9220743c5e829c105dbc65b3ffa809ffd87a80ff9fd8799f409fd8799f401819ffffffffd8799f58203999f2739f0bbcb9727893a0d2e8cae660f0ce36a73e42a58a7102894473a7e0ffffffd8799fd8799fd8799f5820f9d5959ed383550d28c45b30dce80260df7bf4741392f2ec1e3e743aa071c556ff01ffd8799fd8799fd87a9f581c5e40a47ab6e241233bcd9eaede9220743c5e829c105dbc65b3ffa809ffd87a80ff9fd8799f409fd8799f401864ffffffffd8799f5820509b58c2b6fe87f4888de7c11e6ba23ba34a19cadec76b1b7e7904f39ba0608affffffff9fd8799fd8799fd8799f581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3ffd87a80ff9fd8799f409fd8799f401a05f5ab11ffffffffd87a80ffff9fd8799f409fd8799f401936d0ffffffff808080d8799fd8799fd87a9f1b000001739c894e58ffd87a80ffd8799fd87a9f1b000001739c897567ffd87a80ffff9f581c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3ff80d8799f5820a50a890e9f6b1e4ca495b72cc681e5d52061ef26d49bdd075f2fa8c182af1140ffffd87a9fd8799fd8799f58206ee5de7047be901322af0e1ff107ce911237f0b60ea38cd935360cbeca8b1cb3ff01ffffff"
     , Constr 0 [Constr 0 [List [Constr 0 [Constr 0 [Constr 0 [B "\ACK6%\n\239'T\151\180\243\128}f\SUB)\158\&4\229>Z\211\188\DC1\DLE\228=\US4 \188\143\174"],I 6],Constr 0 [Constr 0 [Constr 0 [B "5\222\221)\130\160<\243\158}\206\ETX\200\&9\153O\253\236.\198\176O\FS\242\212\SOa\163"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 100000000]]]],Constr 1 []]],Constr 0 [Constr 0 [Constr 0 [B "\t\194\GS\231\236\229\178$\234\210GuN/\184\f\226\221i\235\CAN\r)(f\DC2\167\197^\192]<"],I 1],Constr 0 [Constr 0 [Constr 1 [B "^@\164z\182\226A#;\205\158\174\222\146 t<^\130\156\DLE]\188e\179\255\168\t"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 100]]]],Constr 0 [B "\STX\170S^\140\133\v@xk\154l\SYN\144r\211\&6\139\143\182x3A=\183\191\137;\221JF\241"]]],Constr 0 [Constr 0 [Constr 0 [B "n\229\222pG\190\144\DC3\"\175\SO\US\241\a\206\145\DC27\240\182\SO\163\140\217\&56\f\190\202\139\FS\179"],I 1],Constr 0 [Constr 0 [Constr 1 [B "^@\164z\182\226A#;\205\158\174\222\146 t<^\130\156\DLE]\188e\179\255\168\t"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 25]]]],Constr 0 [B "9\153\242s\159\v\188\185rx\147\160\210\232\202\230`\240\206\&6\167>B\165\138q\STX\137Ds\167\224"]]],Constr 0 [Constr 0 [Constr 0 [B "\249\213\149\158\211\131U\r(\196[0\220\232\STX`\223{\244t\DC3\146\242\236\RS>t:\160q\197V"],I 1],Constr 0 [Constr 0 [Constr 1 [B "^@\164z\182\226A#;\205\158\174\222\146 t<^\130\156\DLE]\188e\179\255\168\t"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 100]]]],Constr 0 [B "P\155X\194\182\254\135\244\136\141\231\193\RSk\162;\163J\EM\202\222\199k\ESC~y\EOT\243\155\160`\138"]]]],List [Constr 0 [Constr 0 [Constr 0 [B "5\222\221)\130\160<\243\158}\206\ETX\200\&9\153O\253\236.\198\176O\FS\242\212\SOa\163"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 99986193]]]],Constr 1 []]],List [Constr 0 [B "",List [Constr 0 [B "",I 14032]]]],List [],List [],List [],Constr 0 [Constr 0 [Constr 1 [I 1596059111000],Constr 1 []],Constr 0 [Constr 1 [I 1596059120999],Constr 1 []]],List [B "5\222\221)\130\160<\243\158}\206\ETX\200\&9\153O\253\236.\198\176O\FS\242\212\SOa\163"],List [],Constr 0 [B "\165\n\137\SO\159k\RSL\164\149\183,\198\129\229\213 a\239&\212\155\221\a_/\168\193\130\175\DC1@"]],Constr 1 [Constr 0 [Constr 0 [B "n\229\222pG\190\144\DC3\"\175\SO\US\241\a\206\145\DC27\240\182\SO\163\140\217\&56\f\190\202\139\FS\179"],I 1]]]
     )
    ,
     ( "uniswap-6"
     , "d87982d8798a83d87982d87982d8798158200636250aef275497b4f3807d661a299e34e53e5ad3bc1110e43d1f3420bc8fae08d87983d87982d87981581c7f8a76c0ebaa4ad20dfdcd51a5de070ab771f4bf377f2c41e6b71c0ad87a8081d879824081d87982401a05f5e100d87a80d87982d87982d87981582010a7b86c76306acc6dd7aa0002f90309c4e762b74d490364d072d8606e97e2dc01d87983d87982d87981581c7f8a76c0ebaa4ad20dfdcd51a5de070ab771f4bf377f2c41e6b71c0ad87a8081d87982581c60bab65c3fbe2ba8c920d0ba15cdb2489767c81349271ecd5bbbeb7684d8798241411a000f4240d8798241421a000f4240d8798241431a000f4240d8798241441a000f4240d87a80d87982d87982d8798158208e42db798a023468c23085d2cc0018415f16ddc813ae049f8734c2dc7dc7aef002d87983d87982d87a81581c3cbaf97b944fe6e595fed42bfed2b7187978818ecff9cecea5602562d87a8083d879824081d87982401a000186a0d87982581c60bab65c3fbe2ba8c920d0ba15cdb2489767c81349271ecd5bbbeb7681d8798241411a0007a120d87982581c8d823ac08ef4c337a234c64514fe6fa3f5243909109d60d5780a764781d879824a506f6f6c20537461746501d879815820130a5cb91f1f394fa17214a24fb6632bbd8f5f0fbc8a3cface3736b07712bd4783d87983d87982d87981581c7f8a76c0ebaa4ad20dfdcd51a5de070ab771f4bf377f2c41e6b71c0ad87a8083d879824081d87982401a05f5a1d3d87982581c60bab65c3fbe2ba8c920d0ba15cdb2489767c81349271ecd5bbbeb7684d8798241411a000f2eb8d8798241421a000f4240d8798241431a000f4240d8798241441a000f4240d87982581c8d823ac08ef4c337a234c64514fe6fa3f5243909109d60d5780a764781d879824a506f6f6c20537461746500d87a80d87983d87982d87981581c7f8a76c0ebaa4ad20dfdcd51a5de070ab771f4bf377f2c41e6b71c0ad87a8083d87982581c8d823ac08ef4c337a234c64514fe6fa3f5243909109d60d5780a764781d87982582025a10c572c602c02e93897a3e69d63f948647c666104136032b5740fd86aa25a1908bcd879824080d87982581c60bab65c3fbe2ba8c920d0ba15cdb2489767c81349271ecd5bbbeb7680d87a80d87983d87982d87a81581c3cbaf97b944fe6e595fed42bfed2b7187978818ecff9cecea5602562d87a8083d87982581c8d823ac08ef4c337a234c64514fe6fa3f5243909109d60d5780a764781d879824a506f6f6c20537461746501d879824081d87982401a00018a88d87982581c60bab65c3fbe2ba8c920d0ba15cdb2489767c81349271ecd5bbbeb7681d8798241411a0007b4a8d879815820de9e1f3d2534e75e5ce0c2c6ebdd494a9db54c1ab8f1e6592f80221bd745ce9781d879824081d8798240193b4581d87982581c8d823ac08ef4c337a234c64514fe6fa3f5243909109d60d5780a764781d87982582025a10c572c602c02e93897a3e69d63f948647c666104136032b5740fd86aa25a1908bc8080d87982d87982d87980d87a80d87982d87b80d87a8081581c7f8a76c0ebaa4ad20dfdcd51a5de070ab771f4bf377f2c41e6b71c0a82d879825820130a5cb91f1f394fa17214a24fb6632bbd8f5f0fbc8a3cface3736b07712bd47d87a82d87982d87981d879824040d87981d87982581c60bab65c3fbe2ba8c920d0ba15cdb2489767c81349271ecd5bbbeb764141d879811a00036977d879825820de9e1f3d2534e75e5ce0c2c6ebdd494a9db54c1ab8f1e6592f80221bd745ce97d87a82d87982d87981d879824040d87981d87982581c60bab65c3fbe2ba8c920d0ba15cdb2489767c81349271ecd5bbbeb764141d879811a00037233d8798158205f690e6656e0318d065f01c5fc6931495bfbc65d300a95520b34c6772d05fb87d87981581c8d823ac08ef4c337a234c64514fe6fa3f5243909109d60d5780a7647"
     , "d8799fd8799f9fd8799fd8799fd8799f58200636250aef275497b4f3807d661a299e34e53e5ad3bc1110e43d1f3420bc8faeff08ffd8799fd8799fd8799f581c7f8a76c0ebaa4ad20dfdcd51a5de070ab771f4bf377f2c41e6b71c0affd87a80ff9fd8799f409fd8799f401a05f5e100ffffffffd87a80ffffd8799fd8799fd8799f582010a7b86c76306acc6dd7aa0002f90309c4e762b74d490364d072d8606e97e2dcff01ffd8799fd8799fd8799f581c7f8a76c0ebaa4ad20dfdcd51a5de070ab771f4bf377f2c41e6b71c0affd87a80ff9fd8799f581c60bab65c3fbe2ba8c920d0ba15cdb2489767c81349271ecd5bbbeb769fd8799f41411a000f4240ffd8799f41421a000f4240ffd8799f41431a000f4240ffd8799f41441a000f4240ffffffffd87a80ffffd8799fd8799fd8799f58208e42db798a023468c23085d2cc0018415f16ddc813ae049f8734c2dc7dc7aef0ff02ffd8799fd8799fd87a9f581c3cbaf97b944fe6e595fed42bfed2b7187978818ecff9cecea5602562ffd87a80ff9fd8799f409fd8799f401a000186a0ffffffd8799f581c60bab65c3fbe2ba8c920d0ba15cdb2489767c81349271ecd5bbbeb769fd8799f41411a0007a120ffffffd8799f581c8d823ac08ef4c337a234c64514fe6fa3f5243909109d60d5780a76479fd8799f4a506f6f6c20537461746501ffffffffd8799f5820130a5cb91f1f394fa17214a24fb6632bbd8f5f0fbc8a3cface3736b07712bd47ffffffff9fd8799fd8799fd8799f581c7f8a76c0ebaa4ad20dfdcd51a5de070ab771f4bf377f2c41e6b71c0affd87a80ff9fd8799f409fd8799f401a05f5a1d3ffffffd8799f581c60bab65c3fbe2ba8c920d0ba15cdb2489767c81349271ecd5bbbeb769fd8799f41411a000f2eb8ffd8799f41421a000f4240ffd8799f41431a000f4240ffd8799f41441a000f4240ffffffd8799f581c8d823ac08ef4c337a234c64514fe6fa3f5243909109d60d5780a76479fd8799f4a506f6f6c20537461746500ffffffffd87a80ffd8799fd8799fd8799f581c7f8a76c0ebaa4ad20dfdcd51a5de070ab771f4bf377f2c41e6b71c0affd87a80ff9fd8799f581c8d823ac08ef4c337a234c64514fe6fa3f5243909109d60d5780a76479fd8799f582025a10c572c602c02e93897a3e69d63f948647c666104136032b5740fd86aa25a1908bcffffffd8799f4080ffd8799f581c60bab65c3fbe2ba8c920d0ba15cdb2489767c81349271ecd5bbbeb7680ffffd87a80ffd8799fd8799fd87a9f581c3cbaf97b944fe6e595fed42bfed2b7187978818ecff9cecea5602562ffd87a80ff9fd8799f581c8d823ac08ef4c337a234c64514fe6fa3f5243909109d60d5780a76479fd8799f4a506f6f6c20537461746501ffffffd8799f409fd8799f401a00018a88ffffffd8799f581c60bab65c3fbe2ba8c920d0ba15cdb2489767c81349271ecd5bbbeb769fd8799f41411a0007b4a8ffffffffd8799f5820de9e1f3d2534e75e5ce0c2c6ebdd494a9db54c1ab8f1e6592f80221bd745ce97ffffff9fd8799f409fd8799f40193b45ffffffff9fd8799f581c8d823ac08ef4c337a234c64514fe6fa3f5243909109d60d5780a76479fd8799f582025a10c572c602c02e93897a3e69d63f948647c666104136032b5740fd86aa25a1908bcffffffff8080d8799fd8799fd87980d87a80ffd8799fd87b80d87a80ffff9f581c7f8a76c0ebaa4ad20dfdcd51a5de070ab771f4bf377f2c41e6b71c0aff9fd8799f5820130a5cb91f1f394fa17214a24fb6632bbd8f5f0fbc8a3cface3736b07712bd47d87a9fd8799fd8799fd8799f4040ffffd8799fd8799f581c60bab65c3fbe2ba8c920d0ba15cdb2489767c81349271ecd5bbbeb764141ffffffd8799f1a00036977ffffffd8799f5820de9e1f3d2534e75e5ce0c2c6ebdd494a9db54c1ab8f1e6592f80221bd745ce97d87a9fd8799fd8799fd8799f4040ffffd8799fd8799f581c60bab65c3fbe2ba8c920d0ba15cdb2489767c81349271ecd5bbbeb764141ffffffd8799f1a00037233ffffffffd8799f58205f690e6656e0318d065f01c5fc6931495bfbc65d300a95520b34c6772d05fb87ffffd8799f581c8d823ac08ef4c337a234c64514fe6fa3f5243909109d60d5780a7647ffff"
     , Constr 0 [Constr 0 [List [Constr 0 [Constr 0 [Constr 0 [B "\ACK6%\n\239'T\151\180\243\128}f\SUB)\158\&4\229>Z\211\188\DC1\DLE\228=\US4 \188\143\174"],I 8],Constr 0 [Constr 0 [Constr 0 [B "\DEL\138v\192\235\170J\210\r\253\205Q\165\222\a\n\183q\244\191\&7\DEL,A\230\183\FS\n"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 100000000]]]],Constr 1 []]],Constr 0 [Constr 0 [Constr 0 [B "\DLE\167\184lv0j\204m\215\170\NUL\STX\249\ETX\t\196\231b\183MI\ETXd\208r\216`n\151\226\220"],I 1],Constr 0 [Constr 0 [Constr 0 [B "\DEL\138v\192\235\170J\210\r\253\205Q\165\222\a\n\183q\244\191\&7\DEL,A\230\183\FS\n"],Constr 1 []],List [Constr 0 [B "`\186\182\\?\190+\168\201 \208\186\NAK\205\178H\151g\200\DC3I'\RS\205[\187\235v",List [Constr 0 [B "A",I 1000000],Constr 0 [B "B",I 1000000],Constr 0 [B "C",I 1000000],Constr 0 [B "D",I 1000000]]]],Constr 1 []]],Constr 0 [Constr 0 [Constr 0 [B "\142B\219y\138\STX4h\194\&0\133\210\204\NUL\CANA_\SYN\221\200\DC3\174\EOT\159\135\&4\194\220}\199\174\240"],I 2],Constr 0 [Constr 0 [Constr 1 [B "<\186\249{\148O\230\229\149\254\212+\254\210\183\CANyx\129\142\207\249\206\206\165`%b"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 100000]]],Constr 0 [B "`\186\182\\?\190+\168\201 \208\186\NAK\205\178H\151g\200\DC3I'\RS\205[\187\235v",List [Constr 0 [B "A",I 500000]]],Constr 0 [B "\141\130:\192\142\244\195\&7\162\&4\198E\DC4\254o\163\245$9\t\DLE\157`\213x\nvG",List [Constr 0 [B "Pool State",I 1]]]],Constr 0 [B "\DC3\n\\\185\US\US9O\161r\DC4\162O\182c+\189\143_\SI\188\138<\250\206\&76\176w\DC2\189G"]]]],List [Constr 0 [Constr 0 [Constr 0 [B "\DEL\138v\192\235\170J\210\r\253\205Q\165\222\a\n\183q\244\191\&7\DEL,A\230\183\FS\n"],Constr 1 []],List [Constr 0 [B "",List [Constr 0 [B "",I 99983827]]],Constr 0 [B "`\186\182\\?\190+\168\201 \208\186\NAK\205\178H\151g\200\DC3I'\RS\205[\187\235v",List [Constr 0 [B "A",I 995000],Constr 0 [B "B",I 1000000],Constr 0 [B "C",I 1000000],Constr 0 [B "D",I 1000000]]],Constr 0 [B "\141\130:\192\142\244\195\&7\162\&4\198E\DC4\254o\163\245$9\t\DLE\157`\213x\nvG",List [Constr 0 [B "Pool State",I 0]]]],Constr 1 []],Constr 0 [Constr 0 [Constr 0 [B "\DEL\138v\192\235\170J\210\r\253\205Q\165\222\a\n\183q\244\191\&7\DEL,A\230\183\FS\n"],Constr 1 []],List [Constr 0 [B "\141\130:\192\142\244\195\&7\162\&4\198E\DC4\254o\163\245$9\t\DLE\157`\213x\nvG",List [Constr 0 [B "%\161\fW,`,\STX\233\&8\151\163\230\157c\249Hd|fa\EOT\DC3`2\181t\SI\216j\162Z",I 2236]]],Constr 0 [B "",List []],Constr 0 [B "`\186\182\\?\190+\168\201 \208\186\NAK\205\178H\151g\200\DC3I'\RS\205[\187\235v",List []]],Constr 1 []],Constr 0 [Constr 0 [Constr 1 [B "<\186\249{\148O\230\229\149\254\212+\254\210\183\CANyx\129\142\207\249\206\206\165`%b"],Constr 1 []],List [Constr 0 [B "\141\130:\192\142\244\195\&7\162\&4\198E\DC4\254o\163\245$9\t\DLE\157`\213x\nvG",List [Constr 0 [B "Pool State",I 1]]],Constr 0 [B "",List [Constr 0 [B "",I 101000]]],Constr 0 [B "`\186\182\\?\190+\168\201 \208\186\NAK\205\178H\151g\200\DC3I'\RS\205[\187\235v",List [Constr 0 [B "A",I 505000]]]],Constr 0 [B "\222\158\US=%4\231^\\\224\194\198\235\221IJ\157\181L\SUB\184\241\230Y/\128\"\ESC\215E\206\151"]]],List [Constr 0 [B "",List [Constr 0 [B "",I 15173]]]],List [Constr 0 [B "\141\130:\192\142\244\195\&7\162\&4\198E\DC4\254o\163\245$9\t\DLE\157`\213x\nvG",List [Constr 0 [B "%\161\fW,`,\STX\233\&8\151\163\230\157c\249Hd|fa\EOT\DC3`2\181t\SI\216j\162Z",I 2236]]]],List [],List [],Constr 0 [Constr 0 [Constr 0 [],Constr 1 []],Constr 0 [Constr 2 [],Constr 1 []]],List [B "\DEL\138v\192\235\170J\210\r\253\205Q\165\222\a\n\183q\244\191\&7\DEL,A\230\183\FS\n"],List [Constr 0 [B "\DC3\n\\\185\US\US9O\161r\DC4\162O\182c+\189\143_\SI\188\138<\250\206\&76\176w\DC2\189G",Constr 1 [Constr 0 [Constr 0 [Constr 0 [B "",B ""]],Constr 0 [Constr 0 [B "`\186\182\\?\190+\168\201 \208\186\NAK\205\178H\151g\200\DC3I'\RS\205[\187\235v",B "A"]]],Constr 0 [I 223607]]],Constr 0 [B "\222\158\US=%4\231^\\\224\194\198\235\221IJ\157\181L\SUB\184\241\230Y/\128\"\ESC\215E\206\151",Constr 1 [Constr 0 [Constr 0 [Constr 0 [B "",B ""]],Constr 0 [Constr 0 [B "`\186\182\\?\190+\168\201 \208\186\NAK\205\178H\151g\200\DC3I'\RS\205[\187\235v",B "A"]]],Constr 0 [I 225843]]]],Constr 0 [B "_i\SOfV\224\&1\141\ACK_\SOH\197\252i1I[\251\198]0\n\149R\v4\198w-\ENQ\251\135"]],Constr 0 [B "\141\130:\192\142\244\195\&7\162\&4\198E\DC4\254o\163\245$9\t\DLE\157`\213x\nvG"]]
     )
   ]



