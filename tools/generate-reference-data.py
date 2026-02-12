"""Generate reference data from Python for cross-validation with neuromapr R.

Computes deterministic intermediate results using numpy/scipy and optionally
brainsmash, saving them as JSON fixtures for R testthat tests.

Usage:
    python tools/generate-reference-data.py
"""

import json
import os
from pathlib import Path

import numpy as np
from scipy.spatial.distance import pdist, squareform

FIXTURE_DIR = Path(__file__).resolve().parent.parent / "tests" / "testthat" / "fixtures" / "python"


def save_fixture(name, data):
    """Save a fixture as JSON, converting numpy arrays to nested lists."""

    def convert(obj):
        if isinstance(obj, np.ndarray):
            return obj.tolist()
        if isinstance(obj, (np.integer,)):
            return int(obj)
        if isinstance(obj, (np.floating,)):
            return float(obj)
        return obj

    out = {k: convert(v) for k, v in data.items()}
    path = FIXTURE_DIR / f"{name}.json"
    with open(path, "w") as f:
        json.dump(out, f)
    print(f"  saved {path.name}")


def generate_shared_inputs():
    """Create fixed inputs reused across tests."""
    rng = np.random.default_rng(42)
    n = 20
    coords = rng.standard_normal((n, 3))
    distmat = squareform(pdist(coords))
    data = rng.standard_normal(n)

    save_fixture("shared_inputs", {
        "n": n,
        "coords": coords,
        "distmat": distmat,
        "data": data,
    })
    return coords, distmat, data


def generate_weight_matrix_inverse_distance(distmat):
    """Inverse-distance weight matrix, zero diagonal, row-normalised."""
    eps = np.finfo(float).eps
    w = 1.0 / (distmat + eps)
    np.fill_diagonal(w, 0.0)
    rs = w.sum(axis=1, keepdims=True)
    rs[rs == 0] = 1.0
    w = w / rs
    save_fixture("weight_matrix_inverse_distance", {"w": w})
    return w


def generate_weight_matrix_exponential(distmat):
    """Exponential-kernel weight matrix with median bandwidth."""
    nonzero = distmat[distmat > 0]
    bw = float(np.median(nonzero))
    w = np.exp(-distmat / bw)
    np.fill_diagonal(w, 0.0)
    rs = w.sum(axis=1, keepdims=True)
    rs[rs == 0] = 1.0
    w = w / rs
    save_fixture("weight_matrix_exponential", {"w": w, "bandwidth": bw})
    return w


def generate_mem(w):
    """Moran eigenvector maps via double-centred weight matrix."""
    n = w.shape[0]
    centering = np.eye(n) - np.ones((n, n)) / n
    sym_w = (w + w.T) / 2.0
    B = centering @ sym_w @ centering
    eigvals, eigvecs = np.linalg.eigh(B)

    idx = np.argsort(eigvals)[::-1]
    eigvals = eigvals[idx]
    eigvecs = eigvecs[:, idx]

    keep = np.abs(eigvals) > 1e-10
    eigvals = eigvals[keep]
    eigvecs = eigvecs[:, keep]

    save_fixture("mem", {
        "eigenvalues": eigvals,
        "eigenvectors": eigvecs,
    })
    return eigvals, eigvecs


def generate_variogram(data, distmat):
    """Empirical variogram with fixed subsample indices."""
    rng = np.random.default_rng(99)
    n = len(data)
    ns = min(15, n)
    idx = rng.choice(n, size=ns, replace=False).tolist()
    nh = 10
    pv = 50

    sub_data = data[idx]
    sub_dist = distmat[np.ix_(idx, idx)]

    rows, cols = np.triu_indices(len(sub_data), k=1)
    dists = sub_dist[rows, cols]
    diffs_sq = 0.5 * (sub_data[rows] - sub_data[cols]) ** 2

    max_dist = np.percentile(dists, pv)
    mask = dists <= max_dist
    dists = dists[mask]
    diffs_sq = diffs_sq[mask]

    breaks = np.linspace(0, max_dist, nh + 1)
    bins = np.digitize(dists, breaks)
    bins = np.clip(bins, 1, nh)

    bin_centers = (breaks[:-1] + breaks[1:]) / 2.0
    gamma = []
    present_bins = []
    for b in range(1, nh + 1):
        in_bin = bins == b
        if in_bin.any():
            gamma.append(float(diffs_sq[in_bin].mean()))
            present_bins.append(b - 1)

    save_fixture("variogram", {
        "idx": sorted(idx),
        "nh": nh,
        "pv": pv,
        "ns": ns,
        "bin_centers": [float(bin_centers[i]) for i in present_bins],
        "gamma": gamma,
    })


def generate_rotation_matrix():
    """ZYZ Euler angle rotation for specific angles."""
    alpha, beta, gamma = 1.2, 0.8, 2.5
    ca, sa = np.cos(alpha), np.sin(alpha)
    cb, sb = np.cos(beta), np.sin(beta)
    cg, sg = np.cos(gamma), np.sin(gamma)

    Rz1 = np.array([
        [ca, -sa, 0],
        [sa, ca, 0],
        [0, 0, 1],
    ])
    Ry = np.array([
        [cb, 0, sb],
        [0, 1, 0],
        [-sb, 0, cb],
    ])
    Rz2 = np.array([
        [cg, -sg, 0],
        [sg, cg, 0],
        [0, 0, 1],
    ])
    R = Rz2 @ Ry @ Rz1

    save_fixture("rotation_matrix", {
        "alpha": alpha,
        "beta": beta,
        "gamma": gamma,
        "R": R,
    })


def generate_cost_matrix():
    """Squared-distance cost matrix between two coordinate sets."""
    rng = np.random.default_rng(7)
    original = rng.standard_normal((10, 3))
    rotated = rng.standard_normal((10, 3))

    n = original.shape[0]
    cost = np.zeros((n, n))
    for i in range(n):
        diff = rotated - original[i]
        cost[i] = (diff ** 2).sum(axis=1)

    save_fixture("cost_matrix", {
        "original": original,
        "rotated": rotated,
        "cost": cost,
    })


def generate_rank_match():
    """Rank-matching: reorder target to match surrogate ranks."""
    surrogate = np.array([5.0, 3.0, 1.0, 2.0, 4.0])
    target = np.array([10.0, 20.0, 30.0, 40.0, 50.0])

    target_sorted = np.sort(target)
    ranks = np.argsort(np.argsort(surrogate))
    matched = target_sorted[ranks]

    save_fixture("rank_match", {
        "surrogate": surrogate,
        "target": target,
        "matched": matched,
    })


def generate_burt2020_stats():
    """Burt2020 surrogate statistical properties via brainsmash (optional)."""
    try:
        from brainsmash.mapgen.base import Base
    except ImportError:
        print("  skipping burt2020 stats (brainsmash not installed)")
        return

    rng = np.random.default_rng(42)
    n = 30
    coords = rng.standard_normal((n, 3))
    distmat = squareform(pdist(coords))
    data = rng.standard_normal(n)

    gen = Base(
        x=data,
        D=distmat,
        deltas=np.arange(0.1, 1.0, 0.1),
        kernel="exp",
        pv=25,
        nh=25,
        resample=False,
        seed=1,
    )
    surrogates = gen(n=500)

    save_fixture("burt2020_stats", {
        "data": data,
        "distmat": distmat,
        "null_mean": surrogates.mean(axis=0).tolist(),
        "null_sd": surrogates.std(axis=0).tolist(),
        "n_perm": 500,
        "rank_preserved": all(
            np.array_equal(np.sort(surrogates[i]), np.sort(data))
            for i in range(surrogates.shape[0])
        ),
    })


def main():
    os.makedirs(FIXTURE_DIR, exist_ok=True)
    print("Generating Python reference fixtures...")

    coords, distmat, data = generate_shared_inputs()

    print("Deterministic comparisons:")
    w_inv = generate_weight_matrix_inverse_distance(distmat)
    generate_weight_matrix_exponential(distmat)
    generate_mem(w_inv)
    generate_variogram(data, distmat)
    generate_rotation_matrix()
    generate_cost_matrix()
    generate_rank_match()

    print("Statistical comparisons:")
    generate_burt2020_stats()

    print("Done.")


if __name__ == "__main__":
    main()
