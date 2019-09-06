use std::cmp::PartialEq;
use std::fmt;

// define variable
// implement eq and fmt
#[derive(Debug, Clone)]
pub struct Var {
    name: String,
    index: usize,
}

impl Var {
    pub fn new(name: String, index: usize) -> Self {
        Var { name, index }
    }

    pub fn new_x(index: usize) -> Self {
        Var {
            name: format!("X{}", index),
            index,
        }
    }
}

impl PartialEq for Var {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}(#{}))", self.name, self.index)
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

    pub fn to_var(self) -> Option<Var> {
        match self {
            Value::Variable(x) => Some(x),
            _ => None,
        }
    }
}

// Assoc is a binding between var and value
#[derive(Debug, Clone)]
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

// Subst is a group of bindings
#[derive(Debug, Clone)]
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

    pub fn get_a(&self, index: usize) -> Assoc {
        self.s[index].clone()
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
    return false;
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

pub fn fresh(s: &mut State) -> Var {
    let v = Var::new_x(s.get_c());
    s.inc_c();
    println!("fresh new var {:?}", v);
    v
}

pub fn same(v0: Value, v1: Value, s: &mut State) -> bool {
    unify(&v0, &v1, s.get_s())
}

pub fn conj(g0: &Fn(&mut State) -> bool, g1: &Fn(&mut State) -> bool, s: &mut State) -> Vec<State> {
    let mut ret = Vec::new();
    if g0(s) {
        if g1(s) {
            ret.push(s.clone());
        }
    }
    ret
}

pub fn disj(g0: &Fn(&mut State) -> bool, g1: &Fn(&mut State) -> bool, s: &mut State) -> Vec<State> {
    let mut ret = Vec::new();
    let mut s1 = s.clone();
    if g0(s) {
        ret.push(s.clone());
    }

    if g1(&mut s1) {
        ret.push(s1);
    }
    ret
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn var_basic() {
        let a = Var::new("a".to_string(), 0);
        println!("a : {}", a);
        let x1 = Var::new_x(1);
        println!("x1 : {}", x1);
        assert_eq!(a == a, true);
        assert_eq!(x1 == x1, true);
        assert_eq!(a == x1, false);
    }

    #[test]
    fn value_basic() {
        let x0 = Var::new_x(0);
        let v0 = Value::Variable(x0);
        let v1 = Value::Num(1);
        assert_eq!(v0.is_variable(), true);
        assert_eq!(v1.is_variable(), false);
    }

    #[test]
    fn assoc_basic() {
        let x0 = Var::new_x(0);
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
        println!("{:?}", s);
        assert_eq!(ret, true);

        let x0 = Var::new_x(0);
        let v1 = Value::new_var(x0);
        let mut s = Subst::new();
        let ret = unify(&v1, &v1, &mut s);
        println!("{:?}", s);
        assert_eq!(ret, true);

        let mut s = Subst::new();
        let ret = unify(&v0, &v1, &mut s);
        println!("{:?}", s);
        assert_eq!(ret, true);

        let mut s = Subst::new();
        let ret = unify(&v1, &v0, &mut s);
        println!("{:?}", s);
        assert_eq!(ret, true);

        let mut s = Subst::new();
        let v2 = Value::Num(2);
        let ret = unify(&v0, &v2, &mut s);
        println!("{:?}", s);
        assert_eq!(ret, false);
    }

    #[test]
    fn goal_basic() {
        let mut s = State::empty_state();
        let x0 = fresh(&mut s);
        let ret = same(Value::new_var(x0.clone()), Value::new_num(1), &mut s);
        assert_eq!(ret, true);
        let x1 = fresh(&mut s);
        let ret = same(Value::new_var(x0), Value::new_var(x1.clone()), &mut s);
        assert_eq!(ret, true);
        println!("x1 = {:?}", s.lookup(&Value::new_var(x1)));
    }

    fn fives(x: Var, s: &mut State) -> bool {
        same(Value::new_var(x), Value::new_num(5), s)
    }

    fn six(x: Var, s: &mut State) -> bool {
        same(Value::new_var(x), Value::new_num(6), s)
    }

    fn fivesorsix(x: Var, s: &mut State) -> Vec<State> {
        disj(&|s1| fives(x.clone(), s1), &|s2| six(x.clone(), s2), s)
    }

    #[test]
    fn goal_complex() {
        let mut s = State::empty_state();
        let x0 = fresh(&mut s);
        println!("result = {:?}", fivesorsix(x0.clone(), &mut s));
    }
}
