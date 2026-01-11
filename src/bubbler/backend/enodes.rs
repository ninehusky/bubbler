use std::collections::HashMap;

/// I just made some BULLSHIT!

pub type EClassId = usize;

pub type ENodeId = usize;

/// Calling this an ENode is a bit of a misnomer.
/// This is used as a trigger for managing congruence
/// when we implement colored merges.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ENode {
    pub op: &'static str,
    pub children: Vec<EClassId>,
    pub owner: EClassId,
}

#[derive(Default)]
pub struct ENodeRegistry {
    enodes: Vec<ENode>,
    parents: HashMap<EClassId, Vec<ENodeId>>,
}

impl ENodeRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_enode(
        &mut self,
        op: &'static str,
        children: Vec<EClassId>,
        owner: EClassId,
    ) -> ENodeId {
        let id = self.enodes.len();
        self.enodes.push(ENode {
            op,
            children: children.clone(),
            owner: owner.clone(),
        });

        for c in children {
            self.parents.entry(c).or_default().push(id);
        }

        id
    }
}
