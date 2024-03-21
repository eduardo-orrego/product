package com.nttdata.product;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class ProductApplicationTests {

  @Test
  void contextLoads() {
    ProductApplication.main(new String[]{});
    Assertions.assertTrue(true);
  }

}