function [opt_shares, lambda, GW, obj_val, max_SW] = optimize_shares(L, P, pi, aw, SW, p_e, rho, H, gamma, noGW, d, K, prev_shares, kappa)
    max_aw_k = find(aw == max(aw));
    max_SW = L * aw(max_aw_k(1)); 

    if SW > max_SW
        %display(SW)
        SW = max_SW;
        %display(max_SW)
    end

    shares_init = zeros(K, 1) + 0.01;
    %shares_init(max_aw_k(1)) = 1 - 1e-5;
    %shares_init(setdiff(1:end,max_aw_k(1))) = 1e-5/(K+1); 
    
    lb = zeros(K, 1); 
    ub = ones(K, 1); 

    options=optimoptions('fmincon','Display','off','FiniteDifferenceStepSize',1e-7, 'Algorithm', 'interior-point','ScaleProblem',true, ...
        'ConstraintTolerance', 1e-30, 'StepTolerance', 1e-30, 'OptimalityTolerance', 1e-20);
    
    if noGW == 1
        A = [ones(1, K); 
             L * aw];
        b = [1, SW];
        [opt_shares, obj_val, ~, ~, lambdas] = fmincon(@(zz) farmer_obj(zz, L, P, pi, aw, SW, p_e, rho, H, gamma, noGW, d, prev_shares, kappa), ...
                                                 shares_init, A, b, [], [], lb, ub, [], options);
        
        lambda = lambdas.ineqlin(2); 
        GW = 0;
    else
        A = [ones(1, K); 
             -L * aw];
        b = [1, -SW];
        [opt_shares, obj_val, ~, ~, lambdas] = fmincon(@(zz) farmer_obj(zz, L, P, pi, aw, SW, p_e, rho, H, gamma, noGW, d, prev_shares, kappa), ...
                                                 shares_init, A, b, [], [], lb, ub, [], options);
        
        lambda = lambdas.ineqlin(2); 
        GW = L * aw * opt_shares - SW;
    end
    %div_cost = (L/d) * ((opt_shares' * log(opt_shares))  + (1 - sum(opt_shares)) * log(1 - sum(opt_shares)));
    %obj_val = obj_val - div_cost; 