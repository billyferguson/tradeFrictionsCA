function pi = get_pi(nu, opt_shares, aw, p_e, rho, H, gamma, GW, noGW, N, K, T)

    share_fallow = 1 - sum(opt_shares, 2);
    log_rel_shares = log(opt_shares ./ share_fallow);
    mGW = zeros(N, K, T);
    for i = 1:N
        if noGW(i) == 1
                mGW(i, :, :) = 0; 
        else 
            for t = 1:T
                mGW(i, :, t) = aw(i, :, t) .* p_e(i, t) .* rho(i) .* (H(i, t) + gamma(i)*GW(i, t));
            end
        end
    end
    %disp(size(log_rel_shares))
    %disp(size(mGW))
    % I had messed up the sign here, should be + mGW (not - mGW). Notably,
    % this mistake did not ruin estimates of theta, only the cost
    % estimates. Maybe it's true that theta is pinned down by log ratios
    % and prices and not by level shifts ?? not sure exactly. 
    % this is all if i KNOW the true d. if i don't know the true d
    % nailing the mean c gives me the d which gives me the theta, so theta
    % is still fucked under different level shifts of delta. 
    pi = nu*log_rel_shares + mGW;