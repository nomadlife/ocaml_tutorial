module DictionaryIml : Dictionary = struct
type ('k,'v) t = ('k * 'v) list
let empty = []
let insert k v t = (k,v) ::type
let lookup k t = list.assoc k t
end
