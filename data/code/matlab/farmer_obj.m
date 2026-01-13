function objective = farmer_obj(s, L, P, pi, aw, SW, p_e, rho, H, gamma, noGW, d, prev_shares, kappa)
    nonwater_profit = L * (P + pi) * s;
    if noGW == 1
        GW_cost = 0;
    else 
        GW = L * aw * s - SW; 
        GW = max(GW, 0); 
        GW_cost = p_e * rho * (H * GW + 0.5*gamma*(GW^2));
    end
    div_cost = (L/d) * ((s' * log(s))  + (1 - sum(s)) * log(1 - sum(s)));
    acres_switch = sum(abs(s - prev_shares))*L; 
    objective = -(nonwater_profit - GW_cost - div_cost - kappa*(acres_switch));
    %objective = -(nonwater_profit - GW_cost - div_cost - 0.5*kappa*(sum(s)*L)^2);

end



