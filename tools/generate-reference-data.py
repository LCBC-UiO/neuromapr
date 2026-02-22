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
from scipy import stats as sstats
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
    data_y = data + rng.standard_normal(n) * 0.5

    save_fixture("shared_inputs", {
        "n": n,
        "coords": coords,
        "distmat": distmat,
        "data": data,
        "data_y": data_y,
    })
    return coords, distmat, data, data_y


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


def generate_sar_weight_matrix(distmat):
    """SAR exponential weight matrix matching neuromaps _make_weight_matrix."""
    n = len(distmat)
    d0 = float(np.median(distmat[distmat > 0]))
    with np.errstate(over="ignore"):
        w = np.exp(-distmat / d0) * np.logical_not(np.eye(n, dtype=bool))
    with np.errstate(invalid="ignore"):
        w = w / np.sum(w, axis=1, keepdims=True)
    save_fixture("sar_weight_matrix", {"w": w, "d0": d0})
    return w, d0


def generate_moran_double_centered(w):
    """Double-centered weight matrix used for MEM."""
    sym_w = (w + w.T) / 2.0
    row_means = sym_w.mean(axis=1)
    grand_mean = row_means.mean()
    dbl = sym_w - np.add.outer(row_means, row_means) + grand_mean
    save_fixture("moran_double_centered", {"dbl": dbl})
    return dbl


def generate_correlation_reference(data, data_y):
    """Pearson and Spearman correlation reference values."""
    r_p = float(sstats.pearsonr(data, data_y).statistic)
    r_s = float(sstats.spearmanr(data, data_y).statistic)
    save_fixture("correlation_reference", {
        "pearson_r": r_p,
        "spearman_r": r_s,
        "n": len(data),
    })


def _sar_surrogates(distmat, data, d0, seed, n_surr):
    """SAR surrogate generation matching neuromaps."""
    rs = np.random.default_rng(seed)
    n = len(data)
    w = np.exp(-distmat / d0) * np.logical_not(np.eye(n, dtype=bool))
    with np.errstate(invalid="ignore"):
        w = w / np.sum(w, axis=1, keepdims=True)
    rho = 0.5
    iw = np.identity(n) - rho * w
    surrogates = np.zeros((n, n_surr))
    for i in range(n_surr):
        u = rs.standard_normal(n)
        surr = np.linalg.solve(iw, u)
        matched = np.empty_like(surr)
        matched[surr.argsort()] = np.sort(data)
        surrogates[:, i] = matched
    return surrogates


def _moran_surrogates(distmat, data, seed, n_surr):
    """Moran spectral randomization (singleton) matching neuromaps."""
    rs = np.random.default_rng(seed)
    n = len(data)
    w = distmat.copy().astype("float64")
    np.fill_diagonal(w, 1.0)
    w = w ** -1
    np.fill_diagonal(w, 0.0)
    row_sums = w.sum(axis=1, keepdims=True)
    row_sums[row_sums == 0] = 1
    w = w / row_sums

    sym_w = (w + w.T) / 2
    centering = np.eye(n) - np.ones((n, n)) / n
    dbl = centering @ sym_w @ centering

    eigvals, eigvecs = np.linalg.eigh(dbl)
    order = np.argsort(-np.abs(eigvals))
    eigvals = eigvals[order]
    eigvecs = eigvecs[:, order]
    keep = np.abs(eigvals) > 1e-10
    eigvals = eigvals[keep]
    eigvecs = eigvecs[:, keep]

    coeffs = eigvecs.T @ (data - data.mean())
    surrogates = np.zeros((n, n_surr))
    for i in range(n_surr):
        signs = rs.choice([-1, 1], size=len(eigvals))
        surrogates[:, i] = eigvecs @ (coeffs * signs) + data.mean()
    return surrogates


def generate_null_distribution_stats(data, data_y, distmat):
    """Null distribution statistics for SAR and Moran methods."""
    n_perm = 200
    seed = 42
    d0 = float(np.median(distmat[distmat > 0]))

    sar_surr = _sar_surrogates(distmat, data, d0, seed, n_perm)
    sar_null_r = np.array([
        float(sstats.pearsonr(sar_surr[:, i], data_y).statistic)
        for i in range(n_perm)
    ])

    moran_surr = _moran_surrogates(distmat, data, seed, n_perm)
    moran_null_r = np.array([
        float(sstats.pearsonr(moran_surr[:, i], data_y).statistic)
        for i in range(n_perm)
    ])

    save_fixture("null_distribution_stats", {
        "n_perm": n_perm,
        "seed": seed,
        "sar_null_r_mean": float(sar_null_r.mean()),
        "sar_null_r_std": float(sar_null_r.std()),
        "sar_rank_preserved": bool(all(
            np.array_equal(np.sort(sar_surr[:, i]), np.sort(data))
            for i in range(n_perm)
        )),
        "moran_null_r_mean": float(moran_null_r.mean()),
        "moran_null_r_std": float(moran_null_r.std()),
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
        resample=True,
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

    coords, distmat, data, data_y = generate_shared_inputs()

    print("Deterministic comparisons:")
    w_inv = generate_weight_matrix_inverse_distance(distmat)
    generate_weight_matrix_exponential(distmat)
    generate_mem(w_inv)
    generate_moran_double_centered(w_inv)
    generate_variogram(data, distmat)
    generate_rotation_matrix()
    generate_cost_matrix()
    generate_rank_match()
    generate_sar_weight_matrix(distmat)
    generate_correlation_reference(data, data_y)

    print("Statistical comparisons:")
    generate_null_distribution_stats(data, data_y, distmat)
    generate_burt2020_stats()

    print("Done.")


if __name__ == "__main__":
    main()
