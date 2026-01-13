function params = get_approx_params(L, P, pi, aw, SW, max_SW, p_e, rho, H, gamma, noGW, d, K, prev_shares, kappa)

    % THIS IS THE NEW ONE
    if SW > 0 %& max_SW ~= SW
            midSW = SW; 
    else 
        midSW = max_SW/2; 
    end
    sw_counters = [0 midSW max_SW];
    obj_vals = zeros(3, 1);
    index = 1; 
    for sw = sw_counters
        [~, ~, ~, obj_vals(index)] = optimize_shares(L, P, pi, aw, sw, p_e, rho, H, gamma, noGW, d, K, prev_shares, kappa)
        %[~, ~, obj_vals(index)] = get_optimal_shares(L, P, pi, aw, sw, p_e, rho, H, gamma, noGW, d, K, prev_shares, kappa)
        index = index + 1; 
    end
    obj_vals = -obj_vals; 

    c = obj_vals(1); 
    eek = (obj_vals(2) - c)*sw_counters(3)/sw_counters(2); 
    ook = sw_counters(3)^2 - sw_counters(3)*sw_counters(2); 
    a = (obj_vals(3) - c - eek)/(ook);
    b = (obj_vals(2) - c - a*(sw_counters(2)^2))/sw_counters(2); 
    params = [a b c];
end 

