/// A simple directed graph implementation.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct NodeId(usize);

#[allow(dead_code)]
struct Node<N> {
    data: N,
    edges: Vec<NodeId>,
}

pub struct Graph<N> {
    nodes: Vec<Node<N>>,
}

#[allow(dead_code)]
impl<N> Graph<N> {
    pub fn new() -> Self {
        Graph { nodes: Vec::new() }
    }
    pub fn add_node(&mut self, data: N) -> NodeId {
        let id = NodeId(self.nodes.len());

        self.nodes.push(Node {
            data,
            edges: Vec::new(),
        });

        id
    }

    pub fn add_edge(&mut self, from: NodeId, to: NodeId) {
        self.nodes[from.0].edges.push(to);
    }

    pub fn node(&self, id: NodeId) -> &N {
        &self.nodes[id.0].data
    }

    pub fn node_mut(&mut self, id: NodeId) -> &mut N {
        &mut self.nodes[id.0].data
    }

    pub fn children(&self, id: NodeId) -> &[NodeId] {
        &self.nodes[id.0].edges
    }
}

// @maximenko24: this is a fantastic place to get your feet wet with writing tests in Rust.
mod tests {}
