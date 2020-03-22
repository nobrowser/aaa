include String

let hash s =
    let d = Digest.string s in
    Char.(((code d.[3] land 0x3f) lsl 24)
          lor (code d.[7] lsl 16)
          lor (code d.[11] lsl 8)
          lor code d.[13])
