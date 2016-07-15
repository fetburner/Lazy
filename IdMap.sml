structure IdMap = BinaryMapFn (struct
  open Id
  type ord_key = Id.id
end)
