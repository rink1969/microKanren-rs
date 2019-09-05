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
    s: Vec<Assoc>
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
        let mut found = false;
        loop {
            if !ret.is_variable() {
                return ret;
            }
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
}

pub fn unify(v0: &Value, v1: &Value, s: Subst) -> Option<Subst> {
    let u = s.walk(v0);
    let v = s.walk(v1);
    if u.is_variable() && v.is_variable() && u == v {
        return Some(s);
    }
    let mut s = s;
    if u.is_variable() {
        s.ext(u.to_var().unwrap(), v);
        return Some(s);
    }
    if v.is_variable() {
        s.ext(v.to_var().unwrap(), u);
        return Some(s);
    }

    if u == v {
        return Some(s);
    }
    return None;
}

pub fn unify_subst(s0: &Subst, s1: &Subst, s: Subst) -> Option<Subst> {
    if s0.len() != s1.len() {
        return None;
    }
    let mut s = s;
    for i in 0..s0.len() {
        let a0 = s0.get_a(i);
        let a1 = s1.get_a(i);
        // unify var in assoc
        let v0 = Value::new_var(a0.get_var().clone());
        let v1 = Value::new_var(a1.get_var().clone());
        let tmp_s = unify(&v0, &v1, s);
        if tmp_s.is_none() {
            return None;
        }
        s = tmp_s.unwrap();
        // unify value in assoc
        let v0 = a0.get_val().clone();
        let v1 = a1.get_val().clone();
        let tmp_s = unify(&v0, &v1, s);
        if tmp_s.is_none() {
            return None;
        }
        s = tmp_s.unwrap();
    }
    Some(s)
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
        let s = Subst::new();
        let ret = unify(&v0, &v0, s);
        println!("{:?}", ret);
        assert_eq!(ret.is_some(), true);

        let x0 = Var::new_x(0);
        let v1 = Value::new_var(x0);
        let s = Subst::new();
        let ret = unify(&v1, &v1, s);
        println!("{:?}", ret);
        assert_eq!(ret.is_some(), true);

        let s = Subst::new();
        let ret = unify(&v0, &v1, s);
        println!("{:?}", ret);
        assert_eq!(ret.is_some(), true);

        let s = Subst::new();
        let ret = unify(&v1, &v0, s);
        println!("{:?}", ret);
        assert_eq!(ret.is_some(), true);

        let s = Subst::new();
        let v2 = Value::Num(2);
        let ret = unify(&v0, &v2, s);
        println!("{:?}", ret);
        assert_eq!(ret.is_some(), false);
    }

    #[test]
    fn unify_subst_basic() {
        // 1 = x0
        let s = unify(&Value::new_num(1), &Value::new_var(Var::new_x(0)), Subst::new()).unwrap();
        println!("s = {:?}", s);
        // x0 = x1
        let s1 = unify(&Value::new_var(Var::new_x(0)), &Value::new_var(Var::new_x(1)), Subst::new()).unwrap();
        println!("s1 = {:?}", s1);
        // 2 = x1
        let s2 = unify(&Value::new_num(2), &Value::new_var(Var::new_x(1)), Subst::new()).unwrap();
        println!("s2 = {:?}", s2);
        assert_eq!(unify_subst(&s1, &s2, s.clone()).is_some(), false);

        // 1 = x1
        let s3 = unify(&Value::new_num(1), &Value::new_var(Var::new_x(1)), Subst::new()).unwrap();
        println!("s3 = {:?}", s3);
        let s4 = unify_subst(&s1, &s3, s).unwrap();
        println!("s4 = {:?}", s4);
    }
}
