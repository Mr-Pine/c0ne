package edu.kit.kastel.vads.compiler.ir.util;

import edu.kit.kastel.vads.compiler.Span;
import edu.kit.kastel.vads.compiler.ir.IrGraph;
import edu.kit.kastel.vads.compiler.ir.node.Block;
import edu.kit.kastel.vads.compiler.ir.node.Node;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/// Outputs a DOT format string to visualize an [IrGraph].
public class GraphVizPrinter {
    private final Map<Block, Set<Node>> clusters = new HashMap<>();
    private final List<Edge> edges = new ArrayList<>();
    private final Map<Node, Integer> ids = new HashMap<>();
    private final StringBuilder builder = new StringBuilder();
    private final IrGraph graph;
    private int counter = 0;

    public GraphVizPrinter(IrGraph graph) {
        this.graph = graph;
    }

    public static String print(IrGraph graph) {
        GraphVizPrinter printer = new GraphVizPrinter(graph);
        printer.prepare(graph.endBlock(), new HashSet<>());
        printer.print();
        return printer.builder.toString();
    }

    private void prepare(Node node, Set<Node> seen) {
        if (!seen.add(node)) {
            return;
        }

        if (!(node instanceof Block)) {
            this.clusters.computeIfAbsent(node.block(), _ -> Collections.newSetFromMap(new IdentityHashMap<>()))
                .add(node);
        }
        int idx = 0;
        for (Node predecessor : node.predecessors()) {
            this.edges.add(new Edge(predecessor, node, idx++));
            prepare(predecessor, seen);
        }
        if (node == this.graph.endBlock()) {
            this.clusters.put(this.graph.endBlock(), Set.of());
        }
    }

    private void print() {
        this.builder.append("digraph \"")
            .append(this.graph.name())
            .append("\"")
            .append("""
                 {
                    compound=true;
                    layout=dot;
                    node [shape=box];
                    splines=ortho;
                    overlap=false;
                
                """);

        this.clusters.forEach((block, nodes) -> {
            this.builder.append("    subgraph cluster_")
                .append(idFor(block))
                .append(" {\n")
                .repeat(" ", 8)
                .append("c_").append(idFor(block))
                .append(" [width=0, height=0, fixedsize=true, style=invis];\n");
            if (block == this.graph.endBlock()) {
                this.builder.repeat(" ", 8)
                    .append("label=End;\n");
            }
            for (Node node : nodes) {
                this.builder.repeat(" ", 8)
                    .append(idFor(node))
                    .append(" [label=\"")
                    .append(labelFor(node))
                    .append("\"");
                if (node.debugInfo() instanceof DebugInfo.SourceInfo(Span span)) {
                    this.builder.append(", tooltip=\"")
                        .append("source span: ")
                        .append(span)
                        .append("\"");
                }
                this.builder.append("];\n");
            }
            this.builder.append("    }\n\n");
        });

        for (Edge edge : this.edges) {
            this.builder.repeat(" ", 4)
                .append(nameFor(edge.from()))
                .append(" -> ")
                .append(nameFor(edge.to()))
                .append(" [")
                .append("label=")
                .append(edge.idx());

            if (edge.from() instanceof Block b) {
                this.builder.append(", ")
                    .append("ltail=")
                    .append("cluster_")
                    .append(idFor(b));
            }
            if (edge.to() instanceof Block b) {
                this.builder.append(", ")
                    .append("lhead=")
                    .append("cluster_")
                    .append(idFor(b));
            }

            this.builder.append("];\n");
        }

        this.builder.append("}");

    }

    private int idFor(Node node) {
        return this.ids.computeIfAbsent(node, _ -> this.counter++);
    }

    private String nameFor(Node node) {
        if (node instanceof Block) {
            return "c_" + idFor(node);
        }
        return String.valueOf(idFor(node));
    }

    private String labelFor(Node node) {
        return node.toString();
    }

    record Edge(Node from, Node to, int idx) {
    }
}
