# Local AI Build Plan

## Goal
Upgradeable home AI setup for running local LLMs (coding/planning/trading).

## Phase 1: ~$700 — Immediate boost (use existing AM4 PC)

| Part | What | Used Price |
|------|------|-----------|
| GPU | RTX 3090 (24GB VRAM) | $500-600 |
| PSU | 1000W Gold (e.g. Corsair RM1000x) | $100-150 |

- Slots into existing Ryzen 3600X + AM4 board
- Runs Qwen3-Coder-Next, DeepSeek V3.2, or GLM 5.2 at 5-15 tok/s via MoE offload
- Needs PCIe slot + case clearance + PSU cables for one 3090

## Phase 2: ~$1500 — AM4 endgame

| Add | Used Price | Why |
|-----|-----------|-----|
| 5950X (16-core) | $200-300 | Drop-in upgrade on same AM4 board |
| Second RTX 3090 | $500-600 | 48GB pooled VRAM |
| 1600W PSU + bigger case | $200-300 | Power 2x 3090 |

- AM4 board must support PCIe x8/x8 bifurcation (most x570/B550 do)
- Fits Qwen3-72B Q4 entirely in VRAM, fine-tuning capable

## Phase 3: ~$2500+ — EPYC workstation (when AM4 runs out of lanes)

| Part | Used Price | Why |
|------|-----------|-----|
| EPYC 7551P + H11SSL-i board | $250-350 | 128 PCIe lanes, 8-channel DDR4 |
| 8x 32GB DDR4 ECC (256GB) | $150-200 | Room to grow to 384GB+ |
| Move existing 2x 3090s | $0 | Now at full x16 each |

- Supports 4+ GPUs, 1TB+ RAM
- Runs any open-weight model at competitive speed
- This is the endgame for local inference

## Alternative: Single-GPU shortcut

If you never want multi-GPU, skip Phase 2-3 and just buy one 3090. It runs 90% of useful models at good speed. Total cost: ~$700.

## Cheat sheet: What fits where

| Model | Size | 1x 3090 (24GB) | 2x 3090 (48GB) | EPYC CPU-only |
|-------|------|----------------|----------------|---------------|
| Qwen3-Coder-Next (235B MoE) | Q4 ~130GB | MoE offload, 10t/s | MoE offload, 15t/s | CPU, 3-5t/s |
| DeepSeek V3.2 (671B MoE) | Q4 ~370GB | MoE offload, 8t/s | MoE offload, 12t/s | CPU, 3-5t/s |
| GLM 5.2 (753B MoE) | 3-bit ~343GB | MoE offload, 5t/s | MoE offload, 8t/s | CPU, 3-5t/s |
| Qwen3-72B (dense) | Q4 ~41GB | Won't fit | Yes, 25t/s | CPU, 2t/s |
| Qwen3-32B (dense) | Q4 ~18GB | Yes, 35t/s | Yes | CPU, 4t/s |

## Recommendation

Phase 1 alone ($700 for a used 3090) gets you 90% of the value. The EPYC build only matters if you need to run multi-GPU or have extreme RAM requirements for fine-tuning.
