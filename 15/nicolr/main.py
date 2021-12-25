import networkx as nx
import numpy as np

def build_graph(cave_map):
	G = nx.generators.lattice.grid_2d_graph(*cave_map.shape, create_using=nx.DiGraph)
	
	for i in range(cave_map.shape[0]):
		for j in range(cave_map.shape[1]):
			for s in G.successors((i,j)):
				G[s][(i,j)]["risk"] = cave_map[i,j] # Take risk when you leave a tile
	return G

def build_full_map(cave_map, n_tile=5):
	wide_map = np.concatenate([cave_map + i for i in range(n_tile)], axis=1)
	wide_map = np.where(wide_map > 9, wide_map - 9, wide_map)
	big_map = np.concatenate([wide_map + i for i in range(n_tile)], axis=0)
	big_map = np.where(big_map > 9, big_map - 9, big_map)
	return big_map


if __name__ =="__main__":
	with open('input.txt') as f:input_file = f.read().splitlines()
	input_data = list(map(list, input_file))
	cave_map = np.array(input_data).astype(int)

	G = build_graph(cave_map)
	print(f"Part1: {nx.shortest_path_length(G, source=(0,0), target=(cave_map.shape[0] - 1, cave_map.shape[1] - 1), weight='risk')}")

	full_cave_map = build_full_map(cave_map)
	G = build_graph(full_cave_map)
	print(f"Part2: {nx.shortest_path_length(G, source=(0,0), target=(full_cave_map.shape[0] - 1, full_cave_map.shape[1] - 1), weight='risk')}")
