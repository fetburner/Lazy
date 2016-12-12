structure Type : TYPE = struct
  datatype typ =
    (* type variable *)
      VAR of tvar ref
    (* quantified type variable *)
    | META of Id.id
    | CONS of TypeCons.tycons * typ list
  and tvar = UNBOUND of Id.id * int | LINK of typ

  val INT = CONS (TypeCons.INT, [])
  val BOOL = CONS (TypeCons.BOOL, [])
  fun ARROW (t1, t2) = CONS (TypeCons.ARROW, [t1, t2])
  fun LIST t = CONS (TypeCons.LIST, [t])
  fun TUPLE ts = CONS (TypeCons.TUPLE, ts)

  (* type scheme *)
  datatype scheme = TYPE_SCHEME of typ

  fun toTypeScheme t = TYPE_SCHEME t

  fun toString (VAR (ref (LINK t))) =
        toString t
    | toString (VAR (ref (UNBOUND (x, _)))) =
        "'_" ^ Id.toString x
    | toString (META x) =
        "'" ^ Id.toString x
    | toString (CONS (c, ts)) =
        TypeCons.toString c (map toString ts)

  fun schemeToString (TYPE_SCHEME t) = toString t

  (* generate type variable in current level *)
  fun genvar l = VAR (ref (UNBOUND (Id.gensym (), l)))

  (* instantiate type scheme in current level *)
  fun inst l (TYPE_SCHEME t) =
    let 
      val bounds = ref IdMap.empty
      fun instBody (t as (VAR (ref (UNBOUND _)))) = t
        | instBody (VAR (ref (LINK t))) = instBody t
        | instBody (META x) =
            (case IdMap.find (!bounds, x) of
                  SOME t => t
                | NONE =>
                    let val t = genvar l in
                      bounds := IdMap.insert (!bounds, x, t);
                      t
                    end)
        | instBody (CONS (c, ts)) = CONS (c, map instBody ts)
  in
    (instBody t, !bounds)
  end

  (* generalize type variable in current level *)
  fun generalize l t =
    let
      val bounds = ref IdSet.empty
      fun generalizeBody (VAR (ref (LINK t))) =
            generalizeBody t
        | generalizeBody (VAR (r as ref (UNBOUND (x, l')))) =
            if l < l' then
              (bounds := IdSet.add (!bounds, x);
               r := LINK (META x))
            else ()
        | generalizeBody (META _) = ()
        | generalizeBody (CONS (_, ts)) =
            app generalizeBody ts
    in
      generalizeBody t;
      (TYPE_SCHEME t, !bounds)
    end

  (* exception that arises when type checker fails to unify types *)
  exception Unify of typ * typ

  local
    (* occur check *)
    fun occur r1 (VAR (r2 as (ref (UNBOUND _)))) = r1 = r2
      | occur r1 (VAR (r2 as (ref (LINK t2)))) =
          r1 = r2 orelse occur r1 t2
      | occur r1 (META _) = false
      | occur r1 (CONS (_, ts)) = List.exists (occur r1) ts
  in
    (* unifier *)
    fun unify (t1 as CONS (c1, t1s), t2 as CONS (c2, t2s)) =
          if TypeCons.equal (c1, c2) then
            ListPair.appEq unify (t1s, t2s)
            handle ListPair.UnequalLengths => raise (Unify (t1, t2))
          else raise (Unify (t1, t2))
      | unify (VAR (ref (LINK t1)), t2) = unify (t1, t2)
      | unify (t1, VAR (ref (LINK t2))) = unify (t1, t2)
      | unify (t1 as (VAR (r1 as (ref (UNBOUND (_, l1))))),
               t2 as (VAR (r2 as (ref (UNBOUND (_, l2)))))) =
          if r1 = r2 then ()
          else if occur r1 t2 then raise (Unify (t1, t2))
          else if l1 < l2 then r2 := LINK t1
          else r1 := LINK t2
      | unify (t1 as (VAR (r1 as (ref (UNBOUND _)))), t2) =
          if occur r1 t2 then raise (Unify (t1, t2))
          else r1 := LINK t2
      | unify (t1, t2 as (VAR (r2 as (ref (UNBOUND _))))) =
          if occur r2 t1 then raise (Unify (t1, t2))
          else r2 := LINK t1
      | unify (t1, t2) = raise (Unify (t1, t2))
  end
end

