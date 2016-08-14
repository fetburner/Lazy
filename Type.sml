structure Type : TYPE = struct
  datatype typ =
    (* type variable *)
      VAR of tvar ref
    (* quantified type variable *)
    | META of Id.id
    | INT
    | BOOL
    | ARROW of typ * typ
    | TUPLE of typ list
    | LIST of typ
  and tvar = UNBOUND of Id.id * int | LINK of typ

  (* type scheme *)
  datatype scheme = TYPE_SCHEME of typ

  fun toTypeScheme t = TYPE_SCHEME t

  fun toString (VAR (ref (LINK t))) =
        toString t
    | toString (VAR (ref (UNBOUND (x, _)))) =
        "'_" ^ Id.toString x
    | toString (META x) =
        "'" ^ Id.toString x
    | toString INT = "int"
    | toString BOOL = "bool"
    | toString (ARROW (t1, t2)) =
        "(" ^ toString t1 ^ " -> " ^ toString t2 ^ ")"
    | toString (TUPLE []) = "unit"
    | toString (TUPLE (t :: ts)) =
        "(" ^ foldr (fn (t, s) => s ^ " * " ^ toString t) (toString t) ts ^ ")"
    | toString (LIST t) = toString t ^ " list"

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
        | instBody INT = INT
        | instBody BOOL = BOOL
        | instBody (ARROW (t1, t2)) = ARROW (instBody t1, instBody t2)
        | instBody (TUPLE ts) = TUPLE (map instBody ts)
        | instBody (LIST t) = LIST (instBody t)
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
        | generalizeBody INT = ()
        | generalizeBody BOOL = ()
        | generalizeBody (ARROW (t1s, t2)) =
            (generalizeBody t1s; generalizeBody t2)
        | generalizeBody (TUPLE ts) =
            app generalizeBody ts
        | generalizeBody (LIST t) = generalizeBody t
    in
      generalizeBody t;
      (TYPE_SCHEME t, !bounds)
    end

  (* exception that arises when type checker fail to unify types *)
  exception Unify of typ * typ

  local
    (* occur check *)
    fun occur r1 (ARROW (t1, t2)) =
          occur r1 t1 orelse occur r1 t2
      | occur r1 (LIST t) = occur r1 t
      | occur r1 (VAR (r2 as (ref (UNBOUND _)))) = r1 = r2
      | occur r1 (VAR (r2 as (ref (LINK t2)))) =
          r1 = r2 orelse occur r1 t2
      | occur r1 (META _) = false
      | occur r1 (TUPLE ts) = List.exists (occur r1) ts
      | occur r1 INT = false
      | occur r1 BOOL = false
  in
    (* unifier *)
    fun unify (INT, INT) = ()
      | unify (BOOL, BOOL) = ()
      | unify (ARROW (t11, t12), ARROW (t21, t22)) =
          (unify (t11, t21); unify (t12, t22))
      | unify (TUPLE t1s, TUPLE t2s) =
           ListPair.appEq unify (t1s, t2s)
      | unify (LIST t1, LIST t2) = unify (t1, t2)
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

