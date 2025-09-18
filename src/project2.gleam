import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}

pub type NodeState {
  NodeState(
    id: Int,
    s: Float,
    w: Float,
    neighbors: List(Int),
    count: Int,
    ratio: Float,
    algorithm: String,
    round_counts: Int,
    stopped: Bool,
  )
}

pub type SimulationState {
  SimulationState(
    nodes: List(NodeState),
    total_nodes: Int,
    topology: String,
    algorithm: String,
    stopped_count: Int,
  )
}

pub fn create_node(id: Int, algorithm: String) -> NodeState {
  NodeState(
    id: id,
    s: int.to_float(id),
    w: 1.0,
    neighbors: [],
    count: 0,
    ratio: int.to_float(id),
    algorithm: algorithm,
    round_counts: 0,
    stopped: False,
  )
}

pub fn get_random_index(max: Int) -> Int {
  // Simple pseudo-random number generator
  case max {
    0 -> 0
    _ -> {
      let seed = 42
      // Simple fixed seed for demonstration
      seed % max
    }
  }
}

pub fn get_node_at_index(
  nodes: List(NodeState),
  index: Int,
) -> Result(NodeState, Nil) {
  case index, nodes {
    0, [first, ..] -> Ok(first)
    i, [_, ..rest] if i > 0 -> get_node_at_index(rest, i - 1)
    _, [] -> Error(Nil)
    _, _ -> Error(Nil)
  }
}

pub fn update_node_at_index(
  nodes: List(NodeState),
  index: Int,
  new_node: NodeState,
) -> List(NodeState) {
  case index, nodes {
    0, [_, ..rest] -> [new_node, ..rest]
    i, [first, ..rest] if i > 0 -> [
      first,
      ..update_node_at_index(rest, i - 1, new_node)
    ]
    _, [] -> []
    _, _ -> nodes
  }
}

pub fn gossip_step(state: NodeState) -> NodeState {
  case state.stopped {
    True -> state
    False -> {
      case state.count >= 10 {
        True -> NodeState(..state, stopped: True)
        False -> {
          let new_count = state.count + 1
          io.println(
            "Node "
            <> int.to_string(state.id)
            <> " gossip count: "
            <> int.to_string(new_count),
          )
          NodeState(..state, count: new_count)
        }
      }
    }
  }
}

pub fn push_sum_step(
  state: NodeState,
  s_prime: Float,
  w_prime: Float,
) -> NodeState {
  case state.round_counts >= 3 {
    True -> NodeState(..state, stopped: True)
    False -> {
      let s = state.s +. s_prime
      let w = state.w +. w_prime
      let new_ratio = s /. w

      let ratio_diff = case state.ratio >. new_ratio {
        True -> state.ratio -. new_ratio
        False -> new_ratio -. state.ratio
      }

      let round_counts = case ratio_diff <. 0.0000000001 {
        True -> state.round_counts + 1
        False -> 0
      }

      NodeState(
        ..state,
        s: s /. 2.0,
        w: w /. 2.0,
        ratio: new_ratio,
        round_counts: round_counts,
      )
    }
  }
}

pub fn set_neighbors(node: NodeState, neighbors: List(Int)) -> NodeState {
  NodeState(..node, neighbors: neighbors)
}

pub fn setup_full_topology(nodes: List(NodeState)) -> List(NodeState) {
  let all_indices = list.range(0, list.length(nodes) - 1)
  list.map(nodes, fn(node) {
    let other_indices = list.filter(all_indices, fn(i) { i != node.id })
    set_neighbors(node, other_indices)
  })
}

pub fn setup_line_topology(nodes: List(NodeState)) -> List(NodeState) {
  list.index_map(nodes, fn(node, i) {
    let neighbors = case i {
      0 ->
        case list.length(nodes) > 1 {
          True -> [1]
          False -> []
        }
      _ ->
        case i == list.length(nodes) - 1 {
          True -> [i - 1]
          False -> [i - 1, i + 1]
        }
    }
    set_neighbors(node, neighbors)
  })
}

pub fn cube_root(n: Int) -> Int {
  let f = int.to_float(n)
  // Simple approximation for cube root
  case f {
    x if x >=. 1000.0 -> 10
    x if x >=. 512.0 -> 8
    x if x >=. 343.0 -> 7
    x if x >=. 216.0 -> 6
    x if x >=. 125.0 -> 5
    x if x >=. 64.0 -> 4
    x if x >=. 27.0 -> 3
    x if x >=. 8.0 -> 2
    x if x >=. 1.0 -> 1
    _ -> 1
  }
}

pub fn setup_grid_topology(
  nodes: List(NodeState),
  num_nodes: Int,
) -> List(NodeState) {
  let side_len = cube_root(num_nodes)

  list.index_map(nodes, fn(node, i) {
    let side_squared = side_len * side_len
    let x = i / side_squared
    let remainder = i % side_squared
    let y = remainder / side_len
    let z = i % side_len

    let neighbors = {
      let mut_neighbors = []

      // Add all 6 directions (3D grid)
      let x_prev = case x > 0 {
        True -> Some({ x - 1 } * side_len * side_len + y * side_len + z)
        False -> None
      }
      let x_next = case x < side_len - 1 {
        True -> Some({ x + 1 } * side_len * side_len + y * side_len + z)
        False -> None
      }
      let y_prev = case y > 0 {
        True -> Some(x * side_len * side_len + { y - 1 } * side_len + z)
        False -> None
      }
      let y_next = case y < side_len - 1 {
        True -> Some(x * side_len * side_len + { y + 1 } * side_len + z)
        False -> None
      }
      let z_prev = case z > 0 {
        True -> Some(x * side_len * side_len + y * side_len + { z - 1 })
        False -> None
      }
      let z_next = case z < side_len - 1 {
        True -> Some(x * side_len * side_len + y * side_len + { z + 1 })
        False -> None
      }

      let candidates = [x_prev, x_next, y_prev, y_next, z_prev, z_next]

      list.fold(candidates, mut_neighbors, fn(acc, maybe_neighbor) {
        case maybe_neighbor {
          Some(neighbor_id) if neighbor_id < num_nodes -> [neighbor_id, ..acc]
          _ -> acc
        }
      })
    }

    set_neighbors(node, neighbors)
  })
}

pub fn setup_imperfect_grid_topology(
  nodes: List(NodeState),
  num_nodes: Int,
) -> List(NodeState) {
  let grid_nodes = setup_grid_topology(nodes, num_nodes)
  // Add one random neighbor to each node (simplified)
  list.map(grid_nodes, fn(node) {
    let random_neighbor = get_random_index(num_nodes)
    let new_neighbors = case
      list.contains(node.neighbors, random_neighbor)
      || random_neighbor == node.id
    {
      True -> node.neighbors
      False -> [random_neighbor, ..node.neighbors]
    }
    set_neighbors(node, new_neighbors)
  })
}

pub fn setup_topology(
  nodes: List(NodeState),
  num_nodes: Int,
  topology: String,
) -> List(NodeState) {
  case topology {
    "full" -> setup_full_topology(nodes)
    "line" -> setup_line_topology(nodes)
    "3D" -> setup_grid_topology(nodes, num_nodes)
    "imp3D" -> setup_imperfect_grid_topology(nodes, num_nodes)
    _ -> nodes
  }
}

pub fn count_stopped_nodes(nodes: List(NodeState)) -> Int {
  list.fold(nodes, 0, fn(acc, node) {
    case node.stopped {
      True -> acc + 1
      False -> acc
    }
  })
}

pub fn simulate_step(sim_state: SimulationState) -> SimulationState {
  case count_stopped_nodes(sim_state.nodes) >= sim_state.total_nodes {
    True -> sim_state
    False -> {
      let updated_nodes = case sim_state.algorithm {
        "gossip" -> list.map(sim_state.nodes, gossip_step)
        "push-sum" ->
          list.map(sim_state.nodes, fn(node) { push_sum_step(node, 0.0, 0.0) })
        _ -> sim_state.nodes
      }

      let new_stopped_count = count_stopped_nodes(updated_nodes)
      SimulationState(
        ..sim_state,
        nodes: updated_nodes,
        stopped_count: new_stopped_count,
      )
    }
  }
}

pub fn run_simulation(
  num_nodes: Int,
  topology: String,
  algorithm: String,
) -> Int {
  io.println(
    "Starting simulation with "
    <> int.to_string(num_nodes)
    <> " nodes, "
    <> topology
    <> " topology, "
    <> algorithm
    <> " algorithm",
  )

  let nodes =
    list.range(0, num_nodes - 1)
    |> list.map(fn(i) { create_node(i, algorithm) })

  let connected_nodes = setup_topology(nodes, num_nodes, topology)

  let initial_state =
    SimulationState(
      nodes: connected_nodes,
      total_nodes: num_nodes,
      topology: topology,
      algorithm: algorithm,
      stopped_count: 0,
    )

  // Start the first node
  let started_nodes = case connected_nodes {
    [first, ..rest] -> [gossip_step(first), ..rest]
    [] -> []
  }

  let updated_state = SimulationState(..initial_state, nodes: started_nodes)

  // Run simulation for a limited number of steps
  let final_state =
    list.fold(list.range(1, 100), updated_state, fn(state, _) {
      simulate_step(state)
    })

  io.println(
    "Simulation completed. Stopped nodes: "
    <> int.to_string(final_state.stopped_count),
  )
  final_state.stopped_count
}

pub fn main() {
  io.println("Gleam Gossip Simulator")

  // For demonstration, run with default parameters
  let result = run_simulation(10, "full", "gossip")
  io.println("Final result: " <> int.to_string(result) <> " nodes stopped")
}
