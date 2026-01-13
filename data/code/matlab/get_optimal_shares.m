function [opt_shares, GW, obj_val] = get_optimal_shares(L, P, pi, aw, SW, p_e, rho, H, gamma, noGW, d, K, prev_shares, kappa)
    

    shares_init = zeros(K, 1) + 0.01;
    lb = zeros(K, 1); 
    ub = ones(K, 1); 

    options=optimoptions('fmincon','Display','off','FiniteDifferenceStepSize',1e-7, 'Algorithm', 'interior-point','ScaleProblem',true, ...
        'ConstraintTolerance', 1e-30, 'StepTolerance', 1e-30, 'OptimalityTolerance', 1e-20);
    

   A = [ones(1, K); 
        -L * aw];
   b = [1, -SW];
%
    %A = [ones(1, K)];
    %b = [1];
    [opt_shares, obj_val, ~, ~, lambdas] = fmincon(@(zz) farmer_obj(zz, L, P, pi, aw, SW, p_e, rho, H, gamma, noGW, d, prev_shares, kappa), ...
                                             shares_init, A, b, [], [], lb, ub, [], options);
    %
    GW = L * aw * opt_shares - SW;
    %div_cost = (L/d) * ((opt_shares' * log(opt_shares))  + (1 - sum(opt_shares)) * log(1 - sum(opt_shares)));
   % obj_val = obj_val - div_cost; 

   