function obj = get_sw_pi_cor(nu, L, opt_shares, aw, SW, p_e, rho, H, gamma, GW, noGW, N, K, T)
   
    SW_dev = SW - mean(SW, 2); 

    pi = get_pi(nu, opt_shares, aw, p_e, rho, H, gamma, GW, noGW, N, K, T);
    pi_dev = pi - mean(pi, 3); 

    cor = zeros(N, K, T);
    for k = 1:K
        cor(:, k, :) = (squeeze(pi_dev(:, k, :)) .* SW_dev);
    end

    obj = mean(cor, "all");