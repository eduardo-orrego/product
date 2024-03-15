package com.nttdata.product.builder;

import com.nttdata.product.api.request.ProductRequest;
import com.nttdata.product.model.Product;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Objects;

public class ProductBuilder {
    ProductBuilder() {
    }

    public static Product toProductEntity(ProductRequest productRequest) {
        return Product.builder()
            .type(productRequest.getType().name())
            .currency(productRequest.getCurrency().name())
            .interestRate(productRequest.getInterestRate())
            .minimumOpeningAmount(Objects.nonNull(productRequest.getMinimumOpeningAmount())
                ? productRequest.getMinimumOpeningAmount() : BigDecimal.valueOf(0.00))
            .minimumAmountPersonalVIP(Objects.nonNull(productRequest.getMinimumAmountPersonalVIP())
                ? productRequest.getMinimumAmountPersonalVIP() : BigDecimal.valueOf(0.00))
            .maintenanceCommission(Objects.nonNull(productRequest.getMaintenanceCommission())
                ? productRequest.getMaintenanceCommission() : BigDecimal.valueOf(0.00))
            .monthlyLimitMovement(Objects.nonNull(productRequest.getMonthlyLimitMovement())
                ? productRequest.getMonthlyLimitMovement() : 0)
            .limitFreeMovements(Objects.nonNull(productRequest.getLimitFreeMovements())
                ? productRequest.getLimitFreeMovements() : 0)
            .commissionMovement(Objects.nonNull(productRequest.getCommissionMovement())
                ? productRequest.getCommissionMovement() : BigDecimal.valueOf(0.00))
            .specificDayMonthMovement(Objects.nonNull(productRequest.getSpecificDayMonthMovement())
                ? productRequest.getSpecificDayMonthMovement() : 0)
            .dateCreated(LocalDateTime.now())
            .lastUpdated(LocalDateTime.now())
            .build();
    }

    public static Product toProductEntity(ProductRequest productRequest, Product product) {
        return Product.builder()
            .id(product.getId())
            .type(productRequest.getType().name())
            .currency(productRequest.getCurrency().name())
            .interestRate(productRequest.getInterestRate())
            .minimumOpeningAmount(Objects.nonNull(productRequest.getMinimumOpeningAmount())
                ? productRequest.getMinimumOpeningAmount() : product.getMinimumOpeningAmount())
            .minimumAmountPersonalVIP(Objects.nonNull(productRequest.getMinimumAmountPersonalVIP())
                ? productRequest.getMinimumAmountPersonalVIP() : product.getMinimumAmountPersonalVIP())
            .maintenanceCommission(Objects.nonNull(productRequest.getMaintenanceCommission())
                ? productRequest.getMaintenanceCommission() : product.getMaintenanceCommission())
            .monthlyLimitMovement(Objects.nonNull(productRequest.getMonthlyLimitMovement())
                ? productRequest.getMonthlyLimitMovement() : product.getMonthlyLimitMovement())
            .limitFreeMovements(Objects.nonNull(productRequest.getLimitFreeMovements())
                ? productRequest.getLimitFreeMovements() : product.getLimitFreeMovements())
            .commissionMovement(Objects.nonNull(productRequest.getCommissionMovement())
                ? productRequest.getCommissionMovement() : product.getCommissionMovement())
            .specificDayMonthMovement(Objects.nonNull(productRequest.getSpecificDayMonthMovement())
                ? productRequest.getSpecificDayMonthMovement() : product.getSpecificDayMonthMovement())
            .dateCreated(product.getDateCreated())
            .lastUpdated(LocalDateTime.now())
            .build();
    }
}
