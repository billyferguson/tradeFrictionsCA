function [rho, p_e, H, SW, GW, gamma, L, aw, P, shares, ever_grown, noGW, alpha, N, N_d, K, T, unit_info] = load_data()
 
    H = csvread("../../intermediate/matlab/depth_data.csv", 1);
    SW = csvread("../../intermediate/matlab/sw_data.csv", 1);
    rho = csvread("../../intermediate/matlab/rho_data.csv", 1);
    p_e = csvread("../../intermediate/matlab/p_kwh_data.csv", 1);
    %entitlement_shares = csvread("../../intermediate/matlab/entitlement_data.csv", 1);
    gamma = csvread("../../intermediate/matlab/gamma_data.csv", 1);
    L = csvread("../../intermediate/matlab/L_data.csv", 1);
    long_aw = csvread("../../intermediate/matlab/aw_data.csv", 1);
    long_shares = csvread("../../intermediate/matlab/share_data.csv", 1);
    long_ever_grown = csvread("../../intermediate/matlab/ever_grown_data.csv", 1);
    noGW = zeros(size(L, 1), 1); 
    alpha = csvread("../../intermediate/matlab/alpha_data.csv", 1);
    unit_info = csvread("../../intermediate/matlab/farmer_unit_info.csv", 1);

    
    N = size(L, 1); 
    N_d = size(L, 1); 
    %N_u = size(urban_data, 1); 
    T = size(SW, 2); 
    K = size(long_aw, 2);
    
    aw = zeros(N_d, K, T); 
    for t = 1:T
        aw(:, :, t) = long_aw(((t-1)*N_d + 1):(t*N_d), :);
    end
    
    %P = zeros(N, K, T); 
    %for t = 1:T
    %    P(:, :, t) = long_P(((t-1)*N + 1):(t*N), :);
    %end
    
    zero_P = zeros(N_d, K, T); 
    P = zero_P; 
    
    %yield = zeros(N, K, T); 
    %for t = 1:T
    %    yield(:, :, t) = long_yield(((t-1)*N + 1):(t*N), :);
    %end
    
    shares = zeros(N_d, K, T); 
    for t = 1:T
        shares(:, :, t) = long_shares(((t-1)*N_d + 1):(t*N_d), :);
    end
    
    ever_grown = long_ever_grown(1:269, :);
    
    GW = zeros(N_d, T);
    for i = 1:N_d 
        for t = 1:T
            GW(i,t) = L(i, 1) * aw(i, :, t) * shares(i, :, t)' - SW(i, t);
        end
    end



