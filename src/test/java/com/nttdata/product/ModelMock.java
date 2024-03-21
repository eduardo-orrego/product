package com.nttdata.product;

import com.nttdata.product.api.request.ProductRequest;
import com.nttdata.product.enums.CurrencyTypeEnum;
import com.nttdata.product.enums.ProductTypeEnum;
import com.nttdata.product.model.Product;
import java.math.BigDecimal;
import java.time.LocalDateTime;

public class ModelMock {

  public static Product getProduct() {
    return Product.builder()
      .id("id")
      .type(ProductTypeEnum.SAVINGS.name())
      .currency(CurrencyTypeEnum.PEN.name())
      .interestRate(new BigDecimal("10.00"))
      .minimumOpeningAmount(new BigDecimal("10.00"))
      .minimumAmountPersonalVip(new BigDecimal("10.00"))
      .maintenanceCommission(new BigDecimal("10.00"))
      .monthlyLimitMovement(10)
      .limitFreeMovements(10)
      .commissionMovement(new BigDecimal("10.00"))
      .specificDayMonthMovement(10)
      .dateCreated(LocalDateTime.now())
      .lastUpdated(LocalDateTime.now())
      .build();
  }

  public static ProductRequest getProductRequest(){
    return ProductRequest.builder()
      .type(ProductTypeEnum.SAVINGS)
      .currency(CurrencyTypeEnum.PEN)
      .interestRate(new BigDecimal("10.00"))
      .minimumOpeningAmount(new BigDecimal("10.00"))
      .minimumAmountPersonalVip(new BigDecimal("10.00"))
      .maintenanceCommission(new BigDecimal("10.00"))
      .monthlyLimitMovement(10)
      .limitFreeMovements(10)
      .commissionMovement(new BigDecimal("10.00"))
      .specificDayMonthMovement(10)
      .build();
  }


  public static Product getProductDefault() {
    return Product.builder()
      .id("id")
      .type(ProductTypeEnum.SAVINGS.name())
      .currency(CurrencyTypeEnum.PEN.name())
      .interestRate(new BigDecimal("10.00"))
      .minimumOpeningAmount(new BigDecimal("0.00"))
      .minimumAmountPersonalVip(new BigDecimal("0.00"))
      .maintenanceCommission(new BigDecimal("0.00"))
      .monthlyLimitMovement(0)
      .limitFreeMovements(0)
      .commissionMovement(new BigDecimal("0.00"))
      .specificDayMonthMovement(10)
      .dateCreated(LocalDateTime.now())
      .lastUpdated(LocalDateTime.now())
      .build();
  }

  public static ProductRequest getProductRequestDefault(){
    return ProductRequest.builder()
      .type(ProductTypeEnum.SAVINGS)
      .currency(CurrencyTypeEnum.PEN)
      .interestRate(new BigDecimal("10.00"))
      .build();

  }
}
