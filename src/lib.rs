#![allow(non_snake_case)]
use std::fmt;
use std::rc::Rc;

// define variable
// implement eq and fmt
#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    index: usize,
}

impl Var {
    pub fn new(index: usize) -> Self {
        Var { index }
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#({})", self.index)
    }
}

// define Value
// it's a sum type of all possible types
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Variable(Var),
    Num(usize),
}

impl Value {
    pub fn new_var(v: Var) -> Self {
        Value::Variable(v)
    }

    pub fn new_num(v: usize) -> Self {
        Value::Num(v)
    }

    pub fn is_variable(&self) -> bool {
        match self {
            Value::Variable(ref _x) => true,
            _ => false,
        }
    }

    pub fn to_var(&self) -> Option<Var> {
        match self {
            Value::Variable(x) => Some(x.clone()),
            _ => None,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Variable(x) => write!(f, "{}", x),
            Value::Num(x) => write!(f, "{}", x),
        }
    }
}

// Assoc is a binding between var and value
#[derive(Debug, Clone, PartialEq)]
pub struct Assoc {
    var: Var,
    val: Value,
}

impl Assoc {
    pub fn new(var: Var, val: Value) -> Self {
        Assoc { var, val }
    }

    pub fn get_var(&self) -> &Var {
        &self.var
    }

    pub fn get_val(&self) -> &Value {
        &self.val
    }
}

impl fmt::Display for Assoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.var, self.val)
    }
}

// Subst is a group of bindings
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Subst {
    s: Vec<Assoc>,
}

impl Subst {
    pub fn new() -> Subst {
        Subst { s: Vec::new() }
    }

    pub fn is_empty(&self) -> bool {
        self.s.is_empty()
    }

    pub fn len(&self) -> usize {
        self.s.len()
    }

    pub fn walk(&self, v: &Value) -> Value {
        let mut ret = v.clone();
        let mut found;
        loop {
            if !ret.is_variable() {
                return ret;
            }
            found = false;
            for a in &self.s {
                let tmp_v = Value::new_var(a.get_var().clone());
                if tmp_v == ret {
                    ret = a.get_val().clone();
                    found = true;
                    break;
                }
            }
            if !found {
                return ret;
            }
        }
    }

    pub fn ext(&mut self, var: Var, val: Value) {
        let a = Assoc::new(var, val);
        self.s.push(a);
    }

    pub fn clear(&mut self) {
        self.s.clear();
    }
}

impl fmt::Display for Subst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fmt_str = String::new();
        fmt_str.push_str("{");
        for s in self.s.iter() {
            fmt_str.push_str(&s.to_string());
            fmt_str.push_str("; ");
        }
        fmt_str.push_str("}");
        write!(f, "{}", fmt_str)
    }
}

pub fn unify(v0: &Value, v1: &Value, s: &mut Subst) -> bool {
    let u = s.walk(v0);
    let v = s.walk(v1);

    if u.is_variable() && v.is_variable() && u == v {
        return true;
    }
    if u.is_variable() {
        s.ext(u.to_var().unwrap(), v);
        return true;
    }
    if v.is_variable() {
        s.ext(v.to_var().unwrap(), u);
        return true;
    }
    if u == v {
        return true;
    }

    s.clear();
    false
}

// State include a Subst and next variable index
#[derive(Debug, Clone)]
pub struct State {
    s: Subst,
    c: usize,
}

impl State {
    pub fn new(s: Subst, c: usize) -> Self {
        State { s, c }
    }

    pub fn empty_state() -> Self {
        State {
            s: Subst::new(),
            c: 0,
        }
    }

    pub fn get_s(&mut self) -> &mut Subst {
        &mut self.s
    }

    pub fn get_c(&self) -> usize {
        self.c
    }

    pub fn inc_c(&mut self) {
        self.c += 1;
    }

    pub fn set_s(&mut self, s: Subst) {
        self.s = s
    }

    pub fn lookup(&self, v: &Value) -> Value {
        self.s.walk(v)
    }
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fmt_str = String::new();
        fmt_str.push_str("(");
        fmt_str.push_str(&self.s.to_string());
        fmt_str.push_str(")");
        write!(f, "{}", fmt_str)
    }
}

#[derive(Debug, Clone)]
pub struct SStream {
    states: Vec<State>,
}

impl SStream {
    pub fn empty_stream() -> Self {
        SStream { states: Vec::new() }
    }

    pub fn new(states: Vec<State>) -> Self {
        SStream { states }
    }

    pub fn len(&self) -> usize {
        self.states.len()
    }

    pub fn is_empty(&self) -> bool {
        self.states.is_empty()
    }

    pub fn get_states(&self) -> &[State] {
        &self.states
    }

    pub fn get_states_mut(&mut self) -> &mut [State] {
        &mut self.states
    }

    pub fn latest_state(&self) -> &State {
        &self.states[self.states.len() - 1]
    }

    pub fn add(&mut self, state: State) {
        self.states.push(state);
    }
}

impl fmt::Display for SStream {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fmt_str = String::new();
        fmt_str.push_str("[");
        for s in self.states.iter() {
            fmt_str.push_str(&s.to_string());
            fmt_str.push_str(", ");
        }
        fmt_str.push_str("]");
        write!(f, "{}", fmt_str)
    }
}

type Fp = Rc<Box<dyn Fn(&mut State) -> SStream>>;
#[derive(Clone)]
pub struct GoalFn {
    f: Fp,
}

impl GoalFn {
    pub fn new(f: Fp) -> Self {
        GoalFn { f }
    }

    pub fn run(&self, s: &mut State) -> SStream {
        (self.f)(s)
    }
}

type FreshFp = Rc<Box<dyn Fn(Var) -> GoalFn>>;
#[derive(Clone)]
pub struct FreshFn {
    f: FreshFp,
}

impl FreshFn {
    pub fn new(f: FreshFp) -> Self {
        FreshFn { f }
    }

    pub fn run(&self, v: Var) -> GoalFn {
        (self.f)(v)
    }
}

pub fn mplus(ss0: &SStream, ss1: &SStream) -> SStream {
    let mut states = Vec::new();
    states.extend_from_slice(ss0.get_states());
    states.extend_from_slice(ss1.get_states());
    SStream::new(states)
}

pub fn bind(ss0: &mut SStream, g: GoalFn) -> SStream {
    let mut new_ss = SStream::empty_stream();
    for state in ss0.get_states_mut() {
        let tmp_ss = g.run(state);
        new_ss = mplus(&new_ss, &tmp_ss);
    }
    new_ss
}

pub fn callfresh(f: FreshFn) -> GoalFn {
    GoalFn::new(Rc::new(Box::new(move |s| {
        let v = Var::new(s.get_c());
        s.inc_c();
        f.run(v).run(s)
    })))
}

pub fn same(v0: Value, v1: Value) -> GoalFn {
    GoalFn::new(Rc::new(Box::new(move |s| {
        let mut ret = SStream::empty_stream();
        if unify(&v0, &v1, s.get_s()) {
            ret.add(s.clone());
        }
        ret
    })))
}

pub fn conj(g0: GoalFn, g1: GoalFn) -> GoalFn {
    GoalFn::new(Rc::new(Box::new(move |s| {
        let mut ss0 = g0.run(s);
        bind(&mut ss0, g1.clone())
    })))
}

pub fn disj(g0: GoalFn, g1: GoalFn) -> GoalFn {
    GoalFn::new(Rc::new(Box::new(move |s| {
        let mut s1 = s.clone();
        let ss0 = g0.run(s);
        let ss1 = g1.run(&mut s1);
        mplus(&ss0, &ss1)
    })))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn var_basic() {
        let x0 = Var::new(0);
        let x1 = Var::new(1);
        assert_eq!(x0 == x0, true);
        assert_eq!(x1 == x1, true);
        assert_eq!(x0 == x1, false);
    }

    #[test]
    fn value_basic() {
        let x0 = Var::new(0);
        let v0 = Value::Variable(x0);
        let v1 = Value::Num(1);
        assert_eq!(v0.is_variable(), true);
        assert_eq!(v1.is_variable(), false);
    }

    #[test]
    fn assoc_basic() {
        let x0 = Var::new(0);
        let v0 = Value::Num(1);
        let a0 = Assoc::new(x0.clone(), v0.clone());
        assert_eq!(a0.get_var(), &x0);
        assert_eq!(a0.get_val(), &v0);
    }

    #[test]
    fn unify_basic() {
        let v0 = Value::Num(1);
        let mut s = Subst::new();
        let ret = unify(&v0, &v0, &mut s);
        assert_eq!(ret, true);
        assert_eq!(s.is_empty(), true);

        let x0 = Var::new(0);
        let v1 = Value::new_var(x0.clone());
        let mut s = Subst::new();
        let ret = unify(&v1, &v1, &mut s);
        assert_eq!(ret, true);
        assert_eq!(s.is_empty(), true);

        let mut expected_s = Subst::new();
        expected_s.ext(x0.clone(), v0.clone());

        let mut s = Subst::new();
        let ret = unify(&v0, &v1, &mut s);
        assert_eq!(ret, true);
        assert_eq!(s, expected_s);

        let mut s = Subst::new();
        let ret = unify(&v1, &v0, &mut s);
        assert_eq!(ret, true);
        assert_eq!(s, expected_s);

        let mut s = Subst::new();
        let v2 = Value::Num(2);
        let ret = unify(&v0, &v2, &mut s);
        assert_eq!(ret, false);
        assert_eq!(s.is_empty(), true);
    }

    #[test]
    fn goal_basic() {
        let mut s = State::empty_state();
        println!(
            "result = {}",
            callfresh(FreshFn::new(Rc::new(Box::new(move |q| disj(
                same(Value::new_var(q.clone()), Value::new_num(5)),
                same(Value::new_var(q), Value::new_num(6))
            )))))
            .run(&mut s)
        )
    }
}
