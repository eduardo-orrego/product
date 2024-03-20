package com.nttdata.product.builder;

import com.nttdata.product.api.request.ProductRequest;
import com.nttdata.product.model.Product;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Objects;

/**
 * Class: ProductBuilder. <br/>
 * <b>Bootcamp NTTDATA</b><br/>
 *
 * @author NTTDATA
 * @version 1.0
 *   <u>Developed by</u>:
 *   <ul>
 *   <li>Developer Carlos</li>
 *   </ul>
 * @since 1.0
 */
public class ProductBuilder {
  ProductBuilder() {
  }

  public static Product toProductEntity(ProductRequest productRequest) {
    return Product.builder()
      .type(productRequest.getType().name())
      .currency(productRequest.getCurrency().name())
      .interestRate(productRequest.getInterestRate())
      .minimumOpeningAmount(Objects.nonNull(productRequest.getMinimumOpeningAmount())
        ? productRequest.getMinimumOpeningAmount()
        : BigDecimal.valueOf(0.00))
      .minimumAmountPersonalVip(Objects.nonNull(productRequest.getMinimumAmountPersonalVip())
        ? productRequest.getMinimumAmountPersonalVip()
        : BigDecimal.valueOf(0.00))
      .maintenanceCommission(Objects.nonNull(productRequest.getMaintenanceCommission())
        ? productRequest.getMaintenanceCommission()
        : BigDecimal.valueOf(0.00))
      .monthlyLimitMovement(Objects.nonNull(productRequest.getMonthlyLimitMovement())
        ? productRequest.getMonthlyLimitMovement()
        : 0)
      .limitFreeMovements(Objects.nonNull(productRequest.getLimitFreeMovements())
        ? productRequest.getLimitFreeMovements()
        : 0)
      .commissionMovement(Objects.nonNull(productRequest.getCommissionMovement())
        ? productRequest.getCommissionMovement()
        : BigDecimal.valueOf(0.00))
      .specificDayMonthMovement(Objects.nonNull(productRequest.getSpecificDayMonthMovement())
        ? productRequest.getSpecificDayMonthMovement()
        : 0)
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
        ? productRequest.getMinimumOpeningAmount()
        : product.getMinimumOpeningAmount())
      .minimumAmountPersonalVip(Objects.nonNull(productRequest.getMinimumAmountPersonalVip())
        ? productRequest.getMinimumAmountPersonalVip()
        : product.getMinimumAmountPersonalVip())
      .maintenanceCommission(Objects.nonNull(productRequest.getMaintenanceCommission())
        ? productRequest.getMaintenanceCommission()
        : product.getMaintenanceCommission())
      .monthlyLimitMovement(Objects.nonNull(productRequest.getMonthlyLimitMovement())
        ? productRequest.getMonthlyLimitMovement()
        : product.getMonthlyLimitMovement())
      .limitFreeMovements(Objects.nonNull(productRequest.getLimitFreeMovements())
        ? productRequest.getLimitFreeMovements()
        : product.getLimitFreeMovements())
      .commissionMovement(Objects.nonNull(productRequest.getCommissionMovement())
        ? productRequest.getCommissionMovement()
        : product.getCommissionMovement())
      .specificDayMonthMovement(Objects.nonNull(productRequest.getSpecificDayMonthMovement())
        ? productRequest.getSpecificDayMonthMovement()
        : product.getSpecificDayMonthMovement())
      .dateCreated(product.getDateCreated())
      .lastUpdated(LocalDateTime.now())
      .build();
  }
}
