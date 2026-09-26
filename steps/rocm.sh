# AMD GPU / ROCm（--with-rocm のときだけ）。WSL では DKMS（カーネルモジュール）は入れない。
ROCM_INSTALLER_URL="https://repo.radeon.com/amdgpu-install/7.2.1/ubuntu/noble/amdgpu-install_7.2.1.70201-1_all.deb"
export DEBIAN_FRONTEND=noninteractive
if ! dpkg -s amdgpu-install >/dev/null 2>&1; then
    tmp=$(mktemp -d)
    run curl -fsSL -o "$tmp/amdgpu-install.deb" "$ROCM_INSTALLER_URL"
    as_root apt-get install -y -qq "$tmp/amdgpu-install.deb"
    rm -rf "$tmp"
fi
as_root apt-get update -qq
as_root apt-get install -y -qq rocm amdgpu-lib rocm-opencl-runtime rocm-hip-runtime \
    vulkan-tools libvulkan-dev glslc spirv-headers
