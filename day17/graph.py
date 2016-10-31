from Queue import Queue

class Node(object):
    def __init__(self, name):
        self.name = name

class Graph(object):
    """A minimal graph using adjacency lists."""

    def __init__(self, nodes=[], edges=[]):
        self.nodes = nodes
        self.adjacency_list = {n: [tail for (head, tail) in edges if head == n] for n in nodes}

    def successors(self, node):
        """Return a list of successors."""
        return self.adjacency_list[node]

    def predecessor(self, node):
        """Return a list of predecessors."""
        # shown for completeness. If this function is called often, this is the wrong data structure.
        return [head for head in self.nodes if node in self.successors(head)]

    def add_node(self, node):
        raise NotImplementedError()

    def add_edge(self, edge):
        raise NotImplementedError()


def bfs(graph, start):
    remaining_nodes = Queue()
    visited = set()

    def visit(node):
        print(node.name)
        visited.add(node)
        for tail in graph.successors(node):
            if tail not in visited:
                remaining_nodes.put(tail)

    remaining_nodes.put(start)
    while not remaining_nodes.empty():
        n = remaining_nodes.get()
        visit(n)

def spanning_tree(graph, start):
    remaining_nodes = Queue()
    visited = set()

    def visit(node):
        print(node.name)
        visited.add(node)
        for tail in graph.successors(node):
            if tail not in visited:
                remaining_nodes.put(tail)
                tail.parent = node

    remaining_nodes.put(start)
    while not remaining_nodes.empty():
        n = remaining_nodes.get()
        visit(n)

def distance(graph, start):
    remaining_nodes = Queue()
    visited = set()
    distances_dict = {}

    def visit(node):
        print(node.name)
        visited.add(node)
        for tail in graph.successors(node):
            if tail not in visited:
                remaining_nodes.put(tail)
                tail.parent = node
                tail.distance = node.distance + 1
                distances_dict[tail.name] = tail.distance

    remaining_nodes.put(start)
    start.distance = 0
    distances_dict[start.name] = start.distance
    while not remaining_nodes.empty():
        n = remaining_nodes.get()
        visit(n)

    return distances_dict

a = Node('a')
b = Node('b')
c = Node('c')
d = Node('d')
e = Node('e')
g = Graph([a, b, c, d, e], [(a, b), (a, c), (b, d), (b, e), (e, a)])
bfs(g, a)
spanning_tree(g, a)
print(distance(g, a))