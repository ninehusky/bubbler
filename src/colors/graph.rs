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

    pub fn remove_edge(&mut self, from: NodeId, to: NodeId) -> bool {
        if let Some(pos) = self.nodes[from.0].edges.iter().position(|&x| x == to) {
            self.nodes[from.0].edges.remove(pos);
            true
        } else {
            false
        }
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
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_graph_add_node_and_edge() {
        let mut graph = Graph::new();
        let node1 = graph.add_node("Node 1");
        let node2 = graph.add_node("Node 2");

        graph.add_edge(node1, node2);

        assert_eq!(graph.node(node1), &"Node 1");
        assert_eq!(graph.node(node2), &"Node 2");
        assert_eq!(graph.children(node1), &[node2]);
        assert_eq!(graph.children(node2), &[]);
    }

    #[test]
    fn children_ok() {
        let mut graph = Graph::new();
        let node1 = graph.add_node("Node 1");
        let node2 = graph.add_node("Node 2");
        let node3 = graph.add_node("Node 3");

        graph.add_edge(node1, node2);
        graph.add_edge(node1, node3);

        let children = graph.children(node1);
        assert_eq!(children.len(), 2);
        assert!(children.contains(&node2));
        assert!(children.contains(&node3));
    }

    #[test]
    fn add_and_remove_edge() {
        let mut graph = Graph::new();
        let node1 = graph.add_node("Node 1");
        let node2 = graph.add_node("Node 2");

        graph.add_edge(node1, node2);
        assert_eq!(graph.children(node1), &[node2]);

        let removed = graph.remove_edge(node1, node2);
        assert!(removed);
        assert_eq!(graph.children(node1), &[]);

        let not_removed = graph.remove_edge(node1, node2);
        assert!(!not_removed);
    }
}
