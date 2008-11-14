(* Copyright (c) 2007 Vesa Karvonen
 *
 * See the LICENSE file for details.
 *)

local
   signature CAST_REAL = sig
      type real
      structure Bits : WORD
      val toBits : real -> Bits.word
      val fromBits : Bits.word -> real
   end

   structure CastReal64 : CAST_REAL = struct
      type real = Real64.real
      structure Bits = Word64
      fun cast {size = sizeF, set = setF, get = _   }
               {size = sizeT, set = _,    get = getT} =
          if C.S.toWord sizeF <> C.S.toWord sizeT
          then raise Fail "CastReal64: sizes do not match"
          else fn vF => let
                     open C.Ptr
                     val objF = C.new' sizeF
                     val objT = |*! (cast' (inject' (|&! objF)))
                  in
                     setF (objF, vF)
                   ; getT objT before C.discard' objF
                  end
      val word64 = {size = C.S.ulonglong,
                    set = C.Set.ulonglong',
                    get = C.Get.ulonglong'}
      val real64 = {size = C.S.double,
                    set = C.Set.double',
                    get = C.Get.double'}
      val toBits = cast real64 word64
      val fromBits = cast word64 real64
   end

   functor MkPackReal (include CAST_REAL
                       val isBigEndian : bool) : PACK_REAL = struct
      type real = real
      val bytesPerElem = Bits.wordSize div 8
      val isBigEndian = isBigEndian
      val shift = if isBigEndian
                  then fn i => Word.fromInt Bits.wordSize - 0w8 -
                               Word.<< (Word.fromInt i, 0w3)
                  else fn i => Word.<< (Word.fromInt i, 0w3)
      fun tabulator r = let
         val w = toBits r
      in
         fn i => Word8.fromInt (Bits.toIntX (Bits.andb (Bits.>> (w, shift i),
                                                        Bits.fromInt 0xFF)))
      end
      fun sub sub = let
         fun lp (w, i) =
             if i = bytesPerElem
             then fromBits w
             else lp (Bits.orb (w,
                                Bits.<< (Bits.fromInt (Word8.toInt (sub i)),
                                         shift i)),
                      i + 1)
      in
         lp (Bits.fromInt 0, 0)
      end
      fun toBytes r = Word8Vector.tabulate (bytesPerElem, tabulator r)
      fun fromBytes b = sub (fn i => Word8Vector.sub (b, i))
      fun subVec (v, i) =
          sub let val s = i*bytesPerElem in fn i => Word8Vector.sub (v, s+i) end
      fun subArr (a, i) =
          sub let val s = i*bytesPerElem in fn i => Word8Array.sub (a, s+i) end
      fun update (a, i, r) = let
         open Word8ArraySlice
      in
         modifyi (tabulator r o #1)
                 (slice (a, i*bytesPerElem, SOME bytesPerElem))
      end
   end
in
   structure PackReal64Big       : PACK_REAL where type real = Real64.real =
      MkPackReal (open CastReal64 val isBigEndian = true)
   structure PackReal64Little    : PACK_REAL where type real = Real64.real =
      MkPackReal (open CastReal64 val isBigEndian = false)
   structure PackRealBig         : PACK_REAL where type real = Real.real =
      PackReal64Big
   structure PackRealLittle      : PACK_REAL where type real = Real.real =
      PackReal64Little
   structure PackLargeRealBig    : PACK_REAL where type real = LargeReal.real =
      PackReal64Big
   structure PackLargeRealLittle : PACK_REAL where type real = LargeReal.real =
      PackReal64Little
end
