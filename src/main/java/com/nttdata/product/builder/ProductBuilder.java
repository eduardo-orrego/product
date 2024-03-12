package com.nttdata.product.builder;

import com.nttdata.product.api.request.ProductRequest;
import com.nttdata.product.model.Product;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Objects;

public class ProductBuilder {
    ProductBuilder() {
    }

    public static Product toProductEntity(ProductRequest productRequest, String productId) {
        return Product.builder()
            .id(productId)
            .type(productRequest.getType().name())
            .currency(productRequest.getCurrency().name())
            .interestRate(productRequest.getInterestRate())
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
}
