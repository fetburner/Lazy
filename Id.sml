structure Id : ID = struct
  (* identifier *)
  type id = int

  local
    val seed = ref 0
  in
    fun gensym () = 
      (seed := !seed + 1;
       !seed)
  end

  open Int
end
